import torch
from transformers import AutoTokenizer, AutoModelForCausalLM, BitsAndBytesConfig
import os
from peft import PeftModel #QLoRA

class LocalQwenModel:
    def __init__(self, model_path, is_qlora_model=False, base_model_name_or_path=None, initial_examples=None):

        print(f"[LocalQwenModel Init] Initializing LocalQwenModel.")
        self.model_path = os.path.join(os.path.dirname(__file__), model_path)
        print(f"Received model_path argument: {model_path}") # Stampa l'argomento originale
        print(f"Normalized model_path_arg (for adapters/full model): {self.model_path}")
        self.device = "cuda" if torch.cuda.is_available() else "cpu"
        print(f"Using device: {self.device}")

        self.is_qlora_model = is_qlora_model
        self.base_model_name_or_path = base_model_name_or_path

        try:
            self.tokenizer = AutoTokenizer.from_pretrained(self.model_path, trust_remote_code=True)
            print(f"Tokenizer loaded successfully from {self.model_path}")

            if self.is_qlora_model: #QLoRA
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
                    device_map="auto" # Cruciale per QLoRA
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
            print(f"[LocalQwenModel Init] Model setup complete. Effective model device: {self.model.device if hasattr(self.model, 'device') else 'device_map=auto'}")

        except Exception as e:
            print(f"[ERROR][LocalQwenModel Init] Failed to load model/tokenizer: {e}")
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
        formatted_prompt = "<|im_start|>system\nYou are an expert Prolog programming assistant.<|im_end|>\n"
        for entry in self.message_history:
            if entry["role"] == "user":
                formatted_prompt += f"<|im_start|>user\n{entry['content']}<|im_end|>\n"
            elif entry["role"] == "assistant":
                formatted_prompt += f"<|im_start|>assistant\n{entry['content']}{self.tokenizer.eos_token}<|im_end|>\n"
        
        formatted_prompt += f"<|im_start|>user\n{current_user_prompt}<|im_end|>\n<|im_start|>assistant\n"
        return formatted_prompt

    def generate(self, user_prompt_text, add_to_history=True):
        print(f"[LocalQwenModel Generate] Received user prompt (len={len(user_prompt_text)}).")
        
        full_chatml_prompt = self._build_prompt_from_history(user_prompt_text)
        print(f"Full prompt being sent to model (len={len(full_chatml_prompt)}):\n{full_chatml_prompt}") # Print all

        try:
            
            inputs = self.tokenizer([full_chatml_prompt], return_tensors="pt", add_special_tokens=False)
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
            print(f"[LocalQwenModel Generate] Generated response (len={len(decoded_output)}):\n{decoded_output[:500]}...")

            # Add messages to history
            if add_to_history:
                self.message_history.append({"role": "user", "content": user_prompt_text})
                self.message_history.append({"role": "assistant", "content": decoded_output}) # Salva la risposta pulita
                print("  Added to message history.")
            return decoded_output
        
        except Exception as e:
            print(f"[ERROR][LocalQwenModel Generate] Error during generation: {e}")
            import traceback
            traceback.print_exc()
            return f"Error during Qwen generation: {e}"

    def clear_history(self):
        self.message_history = []
        print("[LocalQwenModel] Message history cleared.")