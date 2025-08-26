import torch
from datasets import Dataset
from transformers import AutoTokenizer, AutoModelForCausalLM, BitsAndBytesConfig
from trl import SFTConfig, SFTTrainer
from peft import LoraConfig, get_peft_model
from hl.hl_kb_examples import kb_training_data_samples, kb_evaluation_data_samples
from hl.hl_states_examples import states_training_data_samples, states_evaluation_data_samples
from hl.hl_actions_examples import actions_training_data_samples, actions_evaluation_data_samples
from ll.ll_kb_examples import ll_kb_training_data_samples, ll_kb_evaluation_data_samples
from ll.ll_states_examples import ll_states_training_data_samples, ll_states_evaluation_data_samples
from ll.ll_actions_examples import ll_actions_training_data_samples, ll_actions_evaluation_data_samples
from ll.ll_mappings_examples import ll_mappings_training_data_samples, ll_mappings_evaluation_data_samples
from general.reasoning_examples import reasoning_examples_training_data_samples, reasoning_examples_evalutation_data_samples

# Model definition
#model_name = "LiquidAI/LFM2-350M" #0.5B
#model_name = "google/gemma-3-270m" #0.3B
#model_name = "Qwen/Qwen3-8B" #8B
model_name = "meta-llama/Meta-Llama-3-8B-Instruct" # 8B
tokenizer = AutoTokenizer.from_pretrained(model_name, trust_remote_code=True)
model_type = "unknown"
if model_name:
    model_id_lower = model_name.lower()
    if "gemma" in model_id_lower:
        model_type = "gemma"
    elif "qwen" in model_id_lower:
        model_type = "qwen"
    elif "llama" in model_id_lower:
        model_type = "llama"
print(f"Detected model type for training: {model_type}")

#Chat template
if tokenizer.chat_template is None:
    print("Chat template not found, setting it based on model type.")
    if model_type == "gemma":
        print("Setting Gemma chat template.")
        tokenizer.chat_template = (
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
    elif model_type == "qwen":
        print("Setting Qwen (ChatML) chat template as fallback.")
        # Di solito non è necessario per Qwen, ma è una buona sicurezza
        tokenizer.chat_template = (
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
    elif model_type == "llama":
        print("Setting Llama 3 chat template.")
        tokenizer.chat_template = (
            "{{ bos_token }}"
            "{% for message in messages %}"
                "{% if message['role'] == 'system' %}"
                    "{{ '<|start_header_id|>system<|end_header_id|>\n\n' + message['content'] + '<|eot_id|>' }}"
                "{% elif message['role'] == 'user' %}"
                    "{{ '<|start_header_id|>user<|end_header_id|>\n\n' + message['content'] + '<|eot_id|>' }}"
                "{% elif message['role'] == 'assistant' %}"
                    "{{ '<|start_header_id|>assistant<|end_header_id|>\n\n' + message['content'] + '<|eot_id|>' }}"
                "{% endif %}"
            "{% endfor %}"
            "{% if add_generation_prompt %}"
                "{{ '<|start_header_id|>assistant<|end_header_id|>\n\n' }}"
            "{% endif %}"
        )
    else:
        print("[WARNING] Unknown model type. Prompt formatting might be incorrect.")

# Token padding
if tokenizer.pad_token is None:
    if tokenizer.eos_token is not None:
        tokenizer.pad_token = tokenizer.eos_token
        print(f"Set pad_token to eos_token: {tokenizer.pad_token}")
    else:
        new_pad_token = '[PAD]'
        tokenizer.add_special_tokens({'pad_token': new_pad_token})
        print(f"Added new pad_token: {new_pad_token}")

# Quantization
bnb_config = BitsAndBytesConfig(
    load_in_4bit=True,
    bnb_4bit_quant_type="nf4",
    bnb_4bit_compute_dtype=torch.bfloat16,
    bnb_4bit_use_double_quant=True,
)

# Model creation
model = AutoModelForCausalLM.from_pretrained(
    model_name,
    quantization_config=bnb_config,
    trust_remote_code=True,
    device_map="auto",
    attn_implementation="eager"
)

model.config.use_cache = False
model.config.pretraining_tp = 1

# LoRA Configuration
lora_config = LoraConfig(
    r=32, #Rank
    lora_alpha=32,
    lora_dropout=0.05,
    bias="none",
    task_type="CAUSAL_LM",
    target_modules=["q_proj", "k_proj", "v_proj", "o_proj", "gate_proj", "up_proj", "down_proj"]
)

# Check token padding use
if tokenizer.pad_token == '[PAD]':
    model.resize_token_embeddings(len(tokenizer)) 
    print("Resized token embeddings per il nuovo pad token.")
if tokenizer.pad_token_id is not None:
    model.config.pad_token_id = tokenizer.pad_token_id 
    print(f"Model pad_token_id configured to: {model.config.pad_token_id}")
else:
    print("ERROR: pad_token_id is None after setting pad_token!")

# --- Dataset Aggregation ---
training_data_samples = kb_training_data_samples
evaluation_data_samples = kb_evaluation_data_samples
training_data_samples += states_training_data_samples
evaluation_data_samples += states_evaluation_data_samples
training_data_samples += actions_training_data_samples
evaluation_data_samples += actions_evaluation_data_samples
training_data_samples += ll_kb_training_data_samples
evaluation_data_samples += ll_kb_evaluation_data_samples
training_data_samples += ll_states_training_data_samples
evaluation_data_samples += ll_states_evaluation_data_samples
training_data_samples += ll_actions_training_data_samples
evaluation_data_samples += ll_actions_evaluation_data_samples
training_data_samples += ll_mappings_training_data_samples
evaluation_data_samples += ll_mappings_evaluation_data_samples
training_data_samples += reasoning_examples_training_data_samples
evaluation_data_samples += reasoning_examples_evalutation_data_samples
# --- Dataset Aggregation ---

# Real dataset creation (originale, non formattato)
train_dataset_hf = Dataset.from_list(training_data_samples)
eval_dataset_hf = Dataset.from_list(evaluation_data_samples)

def format_chat_template(example):
    messages = [
        {"role": "system", "content": "You are an expert Prolog programming assistant."},
        {"role": "user", "content": example['natural_language']},
        {"role": "assistant", "content": example['prolog_code']}
    ]
    return {"text": tokenizer.apply_chat_template(messages, tokenize=False)}

train_dataset_hf = train_dataset_hf.map(format_chat_template)
eval_dataset_hf = eval_dataset_hf.map(format_chat_template)

# Formatting examples function
if model_type == "gemma":
    response_template_str = "<start_of_turn>model"
elif model_type == "qwen":
    response_template_str = "<|im_start|>assistant"
elif model_type == "llama":
    # CORRETTO: Questo è l'inizio esatto del turno dell'assistente per Llama 3
    response_template_str = "<|start_header_id|>assistant<|end_header_id|>\n\n"
else:
    # Fallback generico, potrebbe non essere perfetto
    response_template_str = "assistant" 
    
response_template_ids = tokenizer.encode(response_template_str, add_special_tokens=False)
print(f"Response template string: '{response_template_str}'")
print(f"Response template IDs: {response_template_ids}")

sft_training_args = SFTConfig(
    #output_dir="./gemma3_0.3B_qlora_checkpoints",
    num_train_epochs=20,
    per_device_train_batch_size=1,
    per_device_eval_batch_size=1,
    gradient_accumulation_steps=4, #Number of steps before changing the gradient
    gradient_checkpointing=True,
    learning_rate=2e-5,
    weight_decay=0.01,
    warmup_ratio=0.1,
    #logging_dir='./logs_gemma3_0.3B_qlora',
    logging_steps=1, #Number of steps before logging update of the training in the terminal
    eval_strategy="epoch",
    save_strategy="epoch", 
    bf16=torch.cuda.is_available() and torch.cuda.is_bf16_supported(),
    report_to="none", #"wandb" if you want data and graphs about the training
    dataset_text_field="text",
    completion_only_loss=True
)

# Trainer
trainer = SFTTrainer(
    model=model,
    args=sft_training_args,
    train_dataset=train_dataset_hf,
    eval_dataset=eval_dataset_hf,
    peft_config=lora_config
)

# Info pre-training
cuda_available = torch.cuda.is_available()
print(f"CUDA available: {cuda_available}")
if cuda_available:
    print(f"  PyTorch CUDA version: {torch.version.cuda}")
    print(f"  N° GPU: {torch.cuda.device_count()}")
    print(f"  Name GPU: {torch.cuda.get_device_name(torch.cuda.current_device())}")
    print(f"  bf16 supported by GPU: {torch.cuda.is_bf16_supported()}")
    print(f"  Config bf16 training: {sft_training_args.bf16}") 

# Training
print("Start training...")
trainer.train()
print("End training.")

# Save model trained on the dataset
final_qwen_model_path = "./qwen3_8B_qlora_final"
trainer.save_model(final_qwen_model_path) # Save adaptors LoRA
tokenizer.save_pretrained(final_qwen_model_path)
print(f"Adaptors saved at: {final_qwen_model_path}")
