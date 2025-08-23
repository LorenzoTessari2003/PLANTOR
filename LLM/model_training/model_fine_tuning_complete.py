import torch
from datasets import Dataset
from transformers import AutoTokenizer, AutoModelForCausalLM
from trl import SFTConfig, SFTTrainer
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
model_name = "google/gemma-3-270m" #0.3B
#model_name = "Qwen/Qwen3-8B" #8B
tokenizer = AutoTokenizer.from_pretrained(model_name, trust_remote_code=True)
model_type = "unknown"
if model_name:
    model_id_lower = model_name.lower()
    if "gemma" in model_id_lower:
        model_type = "gemma"
    elif "qwen" in model_id_lower:
        model_type = "qwen"
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
    else:
        print("[WARNING] Unknown model type. Prompt formatting might be incorrect.")

#Token padding
print("Token Padding...", flush=True)
if tokenizer.pad_token is None:
    if tokenizer.eos_token is not None:
        tokenizer.pad_token = tokenizer.eos_token
        print(f"Set pad_token to eos_token: {tokenizer.pad_token}", flush=True)
    else:
        new_pad_token = '[PAD]'
        tokenizer.add_special_tokens({'pad_token': new_pad_token})
        print(f"Added new pad_token: {new_pad_token}", flush=True)

# Model settings
print("Model settings...", flush=True)
model = AutoModelForCausalLM.from_pretrained(
    model_name,
    trust_remote_code=True,
    torch_dtype=torch.bfloat16,
    device_map="auto",
    attn_implementation="eager"
)

model.config.use_cache = False
model.config.pretraining_tp = 1

# Check token padding use
print("Checking tokenization...", flush=True)
if tokenizer.pad_token == '[PAD]':
    model.resize_token_embeddings(len(tokenizer))
if tokenizer.pad_token_id is not None:
    model.config.pad_token_id = tokenizer.pad_token_id
    print(f"Model pad_token_id configured to: {model.config.pad_token_id}", flush=True)
else:
    print("ERROR: pad_token_id is None after setting pad_token!", flush=True)

# --- Dataset Aggregation ---
print("Dataset aggregation...", flush=True)
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
else:
    # Fallback generico, potrebbe non essere perfetto
    response_template_str = "assistant" 
    
response_template_ids = tokenizer.encode(response_template_str, add_special_tokens=False)
print(f"Response template string: '{response_template_str}'")
print(f"Response template IDs: {response_template_ids}")

# Training Args
print("Training Args definition...", flush=True)
sft_training_args = SFTConfig(
    output_dir="./qwen3_06B_checkpoints",
    num_train_epochs=10,
    per_device_train_batch_size=1,
    per_device_eval_batch_size=1,
    gradient_accumulation_steps=4, #Number of steps before changing the gradient
    gradient_checkpointing=True,
    learning_rate=2e-5,
    weight_decay=0.01,
    warmup_ratio=0.1,
    logging_dir='./logs_qwen3_06B',
    logging_steps=1, #Number of steps before logging update of the training in the terminal
    eval_strategy="epoch",
    save_strategy="epoch",
    bf16=torch.cuda.is_available() and torch.cuda.is_bf16_supported(),
    report_to="none", #"wandb" if you want data and graphs about the training
    dataset_text_field="text"
)

# Trainer
print("Trainer defintion...", flush=True)
trainer = SFTTrainer(
    model=model,
    args=sft_training_args,
    train_dataset=train_dataset_hf,
    eval_dataset=eval_dataset_hf,
    completion_only_loss = True,
    max_seq_length=4096
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
print("Start training...", flush=True)
trainer.train()
print("End training.", flush=True)

# Save model trained on the dataset
print("Saving model...", flush=True)
final_qwen_model_path = "./qwen3_06B_final"
trainer.save_model(final_qwen_model_path)
tokenizer.save_pretrained(final_qwen_model_path)
print(f"Model and Tokenizer saved at: {final_qwen_model_path}", flush=True)
