import torch
from datasets import Dataset
from transformers import AutoTokenizer, AutoModelForCausalLM, BitsAndBytesConfig
from trl import SFTConfig, SFTTrainer, DataCollatorForCompletionOnlyLM
from peft import LoraConfig, get_peft_model
from hl.hl_kb_examples import kb_training_data_samples, kb_evaluation_data_samples
from hl.hl_states_examples import states_training_data_samples, states_evaluation_data_samples
from hl.hl_actions_examples import actions_training_data_samples, actions_evaluation_data_samples
from ll.ll_kb_examples import ll_kb_training_data_samples, ll_kb_evaluation_data_samples
from ll.ll_states_examples import ll_states_training_data_samples, ll_states_evaluation_data_samples
from ll.ll_actions_examples import ll_actions_training_data_samples, ll_actions_evaluation_data_samples
from ll.ll_mappings_examples import ll_mappings_training_data_samples, ll_mappings_evaluation_data_samples

# Model definition
model_name = "Qwen/Qwen2.5-Coder-0.5B-Instruct" #0.5B
tokenizer = AutoTokenizer.from_pretrained(model_name, trust_remote_code=True)

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
    device_map="auto"
)

model.config.use_cache = False #No cache -> Memory saver for better performance
model.config.pretraining_tp = 1

# LoRA Configuration
lora_config = LoraConfig(
    r=16, #Rank
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
# --- Dataset Aggregation ---

# Real dataset creation (originale, non formattato)
train_dataset_hf = Dataset.from_list(training_data_samples)
eval_dataset_hf = Dataset.from_list(evaluation_data_samples)

# Formatting examples function
def formatting_prompt_for_sft(example):
    nl = example['natural_language']
    pl = example['prolog_code']
    text = f"<|im_start|>system\nYou are an expert Prolog programming assistant.<|im_end|>\n<|im_start|>user\n{nl}<|im_end|>\n<|im_start|>assistant\n{pl}{tokenizer.eos_token}"
    return text # return single formatted string
response_template_str = "<|im_start|>assistant"
response_template_ids = tokenizer.encode(response_template_str, add_special_tokens=False)
print(f"Response template string: '{response_template_str}'")
print(f"Response template IDs: {response_template_ids}")

sft_training_args = SFTConfig(
    output_dir="./qwen_coder_prolog_finetuned_qlora",
    num_train_epochs=5,
    per_device_train_batch_size=1,
    per_device_eval_batch_size=1,
    gradient_accumulation_steps=1, #Number of steps before changing the gradient
    learning_rate=2e-5,
    weight_decay=0.01,
    warmup_ratio=0.1,
    logging_dir='./logs_qwen_coder_qlora',
    logging_steps=1, #Number of steps before logging update of the training in the terminal
    eval_strategy="epoch",
    save_strategy="epoch", 
    bf16=torch.cuda.is_available() and torch.cuda.is_bf16_supported(),
    report_to="none", #"wandb" if you want data and graphs about the training
    dataset_text_field="text",
    max_seq_length=4096, 
)

# Data Collator
data_collator_LM = DataCollatorForCompletionOnlyLM(response_template=response_template_ids, tokenizer=tokenizer)

# Trainer
trainer = SFTTrainer(
    model=model,
    args=sft_training_args,
    train_dataset=train_dataset_hf,
    eval_dataset=eval_dataset_hf,
    formatting_func=formatting_prompt_for_sft, 
    data_collator=data_collator_LM,
    peft_config=lora_config,
)

# Stampa info prima del training
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
final_qwen_model_path = "./qwen_coder_prolog_qlora_finetuned_final"
trainer.save_model(final_qwen_model_path) # Save adaptors LoRA
tokenizer.save_pretrained(final_qwen_model_path)
print(f"Adaptors saved at: {final_qwen_model_path}")