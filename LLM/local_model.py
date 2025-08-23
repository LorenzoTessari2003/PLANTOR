import torch
from transformers import AutoTokenizer, AutoModelForCausalLM, BitsAndBytesConfig
import os
from peft import PeftModel #QLoRA

class LocalModel:
    def __init__(self, model_path, is_qlora_model=False, base_model_name_or_path=None, initial_examples=None):

        print(f"[LocalModel Init] Initializing LocalModel.")
        self.model_path = os.path.join(os.path.dirname(__file__), model_path)
        print(f"Received model_path argument: {model_path}") 
        print(f"Normalized model_path_arg (for adapters/full model): {self.model_path}")
        self.device = "cuda" if torch.cuda.is_available() else "cpu"
        print(f"Using device: {self.device}")

        self.is_qlora_model = is_qlora_model
        self.base_model_name_or_path = base_model_name_or_path

        self.model_type = "unknown"
        model_id_for_type_check = self.base_model_name_or_path if self.is_qlora_model else self.model_path
        if model_id_for_type_check:
            model_id_lower = model_id_for_type_check.lower()
            if "gemma" in model_id_lower:
                self.model_type = "gemma"
            elif "qwen" in model_id_lower:
                self.model_type = "qwen"
        print(f"Detected model type: {self.model_type}")

        try:
            self.tokenizer = AutoTokenizer.from_pretrained(self.model_path, trust_remote_code=True)
            print(f"Tokenizer loaded successfully from {self.model_path}")

            if self.tokenizer.chat_template is None:
                if self.model_type == "gemma":
                    print("Setting Gemma chat template.")
                    self.tokenizer.chat_template = (
                        "{% for message in messages %}"
                        "{% if message['role'] == 'user' %}"
                        "{{ '<start_of_turn>user\n' + message['content'] + '<end_of_turn>\n' }}"
                        "{% elif message['role'] == 'assistant' %}"
                        "{{ '<start_of_turn>model\n' + message['content'] + '<end_of_turn>\n' }}"
                        "{% elif message['role'] == 'system' %}"
                        "{{ message['content'] + '\n' }}"
                        "{% endif %}"
                        "{% endfor %}"
                        "{% if add_generation_prompt %}"
                        "{{ '<start_of_turn>model\n' }}"
                        "{% endif %}"
                    )
                elif self.model_type == "qwen":
                    # Nota: i tokenizer di Qwen di solito hanno già il template. 
                    # Questo è solo un fallback di sicurezza.
                    print("Setting Qwen (ChatML) chat template.")
                    self.tokenizer.chat_template = (
                        "{% for message in messages %}"
                        "{% if message['role'] == 'system' %}"
                        "{{'<|im_start|>system\n' + message['content'] + '<|im_end|>\n'}}"
                        "{% elif message['role'] == 'user' %}"
                        "{{'<|im_start|>user\n' + message['content'] + '<|im_end|>\n'}}"
                        "{% elif message['role'] == 'assistant' %}"
                        "{{'<|im_start|>assistant\n' + message['content'] + '<|im_end|>\n'}}"
                        "{% endif %}"
                        "{% endfor %}"
                        "{% if add_generation_prompt %}"
                        "{{ '<|im_start|>assistant\n' }}"
                        "{% endif %}"
                    )
                else:
                    print("[WARNING] Unknown model type. No chat template will be set. Prompt formatting might be incorrect.")

            if self.is_qlora_model:
                if not self.base_model_name_or_path:
                    raise ValueError("base_model_name_or_path must be provided for QLoRA models.")
                
                print(f"Loading QLoRA model. Base model: {self.base_model_name_or_path}, Adapters: {self.model_path}")
                bnb_config = BitsAndBytesConfig(
                    load_in_4bit=True,
                    bnb_4bit_quant_type="nf4",
                    bnb_4bit_compute_dtype=torch.bfloat16,
                    bnb_4bit_use_double_quant=True,
                )
                base_model = AutoModelForCausalLM.from_pretrained(
                    self.base_model_name_or_path,
                    quantization_config=bnb_config,
                    trust_remote_code=True,
                    device_map="auto",
                    attn_implementation="eager"
                )
                self.model = PeftModel.from_pretrained(base_model, self.model_path)
                print(f"QLoRA model (base + adapters) loaded successfully.")
            else: #Full Fine-Tuning
                print(f"Loading Full Fine-Tuned model from {self.model_path}")
                self.model = AutoModelForCausalLM.from_pretrained(
                    self.model_path,
                    trust_remote_code=True,
                    torch_dtype=torch.bfloat16,
                    device_map="auto"
                )
                print(f"Full Fine-Tuned model loaded successfully.")

            # Pad tokenizer
            if self.tokenizer.pad_token_id is None and self.tokenizer.eos_token_id is not None:
                self.tokenizer.pad_token_id = self.tokenizer.eos_token_id
            
            if self.tokenizer.pad_token_id is not None:
                 self.model.config.pad_token_id = self.tokenizer.pad_token_id
                 
            self.model.eval() # Mode eval
            print(f"[LocalModel Init] Model setup complete. Effective model device: {self.model.device if hasattr(self.model, 'device') else 'device_map=auto'}")

        except Exception as e:
            print(f"[ERROR][LocalModel Init] Failed to load model/tokenizer: {e}")
            raise

        self.max_new_tokens = 4096
        self.num_beams = 1
        self.temperature = 0.01
        self.top_p = 0.95
        self.do_sample = True
        if self.temperature < 0.01:
            self.do_sample = False
            self.num_beams = 1
        self.message_history = []

    def _build_prompt_from_history(self, current_user_prompt):
        messages = [
            {"role": "system", "content": "You are an expert Prolog programming assistant."}
        ]
        messages.extend(self.message_history)
        messages.append({"role": "user", "content": current_user_prompt})

        if self.tokenizer.chat_template:
            formatted_prompt = self.tokenizer.apply_chat_template(
                messages, 
                tokenize=False, 
                add_generation_prompt=True
            )
            return formatted_prompt
        else:
            print("[WARNING] Chat template not set. Using a basic prompt format.")
            full_prompt = "system\nYou are an expert Prolog programming assistant.\n"
            for msg in self.message_history:
                full_prompt += f"{msg['role']}\n{msg['content']}\n"
            full_prompt += f"user\n{current_user_prompt}\nassistant\n"
            return full_prompt

    def generate(self, user_prompt_text, add_to_history=True):
        print(f"[LocalModel Generate] Received user prompt (len={len(user_prompt_text)}).")
        
        full_chatml_prompt = self._build_prompt_from_history(user_prompt_text)
        print(f"Full prompt being sent to model (len={len(full_chatml_prompt)}):\n{full_chatml_prompt}")

        try:
            
            inputs = self.tokenizer(full_chatml_prompt, return_tensors="pt") 
            input_ids = inputs.input_ids.to(self.device)
            attention_mask = inputs.attention_mask.to(self.device) if 'attention_mask' in inputs else None

            print(f"  Input token length: {input_ids.shape[1]}")
            print(f"  Generating response with max_new_tokens={self.max_new_tokens}...")
            
            with torch.no_grad(): 
                outputs = self.model.generate(
                    input_ids,
                    attention_mask=attention_mask,
                    max_new_tokens=self.max_new_tokens,
                    num_beams=self.num_beams,
                    temperature=self.temperature if self.do_sample else 1.0,
                    top_p=self.top_p if self.do_sample else 1.0,
                    do_sample=self.do_sample,
                    early_stopping=True if self.num_beams > 1 else False,
                    eos_token_id=self.tokenizer.eos_token_id,
                    pad_token_id=self.tokenizer.pad_token_id
                )
            
            generated_ids = outputs[0][input_ids.shape[1]:]
            decoded_output = self.tokenizer.decode(generated_ids, skip_special_tokens=True)
            print("--- INIZIO OUTPUT COMPLETO DAL MODELLO ---")
            print(decoded_output)
            print("--- FINE OUTPUT COMPLETO DAL MODELLO ---")

            # Add messages to history
            if add_to_history:
                self.message_history.append({"role": "user", "content": user_prompt_text})
                self.message_history.append({"role": "assistant", "content": decoded_output}) # Salva la risposta pulita
                print("  Added to message history.")
            return decoded_output
        
        except Exception as e:
            print(f"[ERROR][LocalModel Generate] Error during generation: {e}")
            import traceback
            traceback.print_exc()
            return f"Error during generation: {e}"

    def clear_history(self):
        self.message_history = []
        print("[LocalModel] Message history cleared.")