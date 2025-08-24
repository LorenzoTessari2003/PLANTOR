import yaml
import os
import requests
import json
from dotenv import load_dotenv
import time
import traceback

try:
    import utility
except ImportError:
    from . import utility
from .local_model import LocalModel

class LLM:
    llm_default_config = {
        "max_tokens": 4096,
        "temperature": 0.1,
        "top_p": 1.0,
        "frequency_penalty": 0.0,
        "presence_penalty": 0.0,
        "stop": None,
        "seed": 0
    }

    def __init__(self, llm_connection_config_file = os.path.join(os.path.dirname(__file__), "conf/gpt4o.yaml"), examples_yaml_file = [""], llm_config = None):
        print("[LLM Init] Starting LLM initialization...") 
        load_dotenv()

        self.initial_examples = [] #Examples for OpenAI and Ollama
        self.local_instance = None # Model local instance

        if examples_yaml_file:
            if isinstance(examples_yaml_file, str):
                examples_yaml_file = [examples_yaml_file]
            
            print(f"[LLM Init] Loading examples from: {examples_yaml_file}")
            try:
                for file_path in examples_yaml_file:
                    if file_path and os.path.isfile(file_path):
                        print(f"[LLM Init] Processing example file: {file_path}")
                        tmp_messages_for_file = []
                        utility.includeYAML(file_path, tmp_messages_for_file, "")
                        self.initial_examples.extend(tmp_messages_for_file)
                    else:
                        print(f"[LLM Init] Skipping non-existent or empty example file: {file_path}")
                
                initial_message_count = len(self.initial_examples)
                print(f"[LLM Init] Loaded {initial_message_count} initial example messages.")
                if initial_message_count > 0:
                    print("[LLM Init] First few initial example messages:")
                    for i, msg in enumerate(self.initial_examples[:3]):
                         print(f"  [{i}] Role: {msg.get('role')}, Content: {str(msg.get('content', ''))[:100]}...")
                    if initial_message_count > 3:
                        print("  ...")
            except Exception as e:
                print(f"[ERROR][LLM Init] Failed to load examples: {e}")

        print(f"[LLM Init] Loading LLM connection config from: {llm_connection_config_file}")
        if llm_connection_config_file.endswith(".yaml") and os.path.isfile(llm_connection_config_file):
            with open(llm_connection_config_file) as file:
                llm_connection_config = yaml.load(file, Loader=yaml.FullLoader)

                self.API_TYPE = llm_connection_config.get("API_TYPE", "azure_openai")
                print(f"[LLM Init] API_TYPE detected: {self.API_TYPE}")

                if self.API_TYPE == "azure_openai":
                    self.engine       = llm_connection_config.get("LLM_VERSION", "N/A")
                    self.API_KEY_NAME = llm_connection_config.get("API_KEY_NAME", "N/A")
                    self.ENDPOINT     = llm_connection_config.get("ENDPOINT", "N/A")
                    self.API_VERSION  = llm_connection_config.get("API_VERSION", "N/A")
                    print(f"[LLM Init] Azure Config - Engine: {self.engine}, Endpoint: {self.ENDPOINT}, API Version: {self.API_VERSION}")
                elif self.API_TYPE == "ollama":
                    self.BASE_URL   = llm_connection_config.get("BASE_URL")
                    self.MODEL_NAME = llm_connection_config.get("MODEL_NAME")
                    self.engine = self.MODEL_NAME
                    print(f"[LLM Init] Ollama Config - Base URL: {self.BASE_URL}, Model Name: {self.MODEL_NAME}")
                    if not self.BASE_URL or not self.MODEL_NAME:
                         print("[WARNING][LLM Init] BASE_URL or MODEL_NAME missing for Ollama configuration!")
                elif self.API_TYPE == "local": 
                    self.MODEL_NAME = llm_connection_config.get("MODEL_NAME_DISPLAY", "local_qwen_default")
                    model_path = llm_connection_config.get("MODEL_PATH")
                    is_qlora = llm_connection_config.get("IS_QLORA_MODEL", False)
                    base_model_name = llm_connection_config.get("BASE_MODEL_NAME_OR_PATH", None)

                    if not model_path:
                        raise ValueError("[LLM Init] MODEL_PATH missing for local_qwen configuration!")
                    if is_qlora and not base_model_name:
                        raise ValueError("[LLM Init] BASE_MODEL_NAME_OR_PATH missing for local_qwen QLoRA configuration!")
                    
                    print(f"[LLM Init] Local Config - Display Name: {self.MODEL_NAME}, Path: {model_path}, Is QLoRA: {is_qlora}, Base Model: {base_model_name}")
                    self.local_instance = LocalModel(
                        model_path=model_path,
                        is_qlora_model=is_qlora,
                        base_model_name_or_path=base_model_name,
                        initial_examples=self.initial_examples
                    )
                    
                    #Specific config parameters
                    if "MAX_NEW_TOKENS" in llm_connection_config:
                        self.local_instance.max_new_tokens = llm_connection_config["MAX_NEW_TOKENS"]
                    if "NUM_BEAMS" in llm_connection_config:
                        self.local_instance.num_beams = llm_connection_config["NUM_BEAMS"]
                    if "TEMPERATURE" in llm_connection_config:
                        self.local_instance.temperature = llm_connection_config["TEMPERATURE"]
                        if self.local_instance.temperature < 0.01: 
                             self.local_instance.do_sample = False
                             self.local_instance.num_beams = 1 # Greedy se temp è ~0
                        else:
                             self.local_instance.do_sample = True

                    if "TOP_P" in llm_connection_config: # Sovrascrive quello globale
                        self.local_instance.top_p = llm_connection_config["TOP_P"]
                    if "DO_SAMPLE" in llm_connection_config:
                        self.local_instance.do_sample = llm_connection_config["DO_SAMPLE"]

                else:
                    raise ValueError(f"API_TYPE non valido: {self.API_TYPE}. Deve essere 'azure_openai', 'ollama', o 'local'.")
        else:
            raise FileNotFoundError(f"The selected file {llm_connection_config_file} does not exist or is not a yaml file")

        self.__config(llm_config) # Per parametri di default OpenAI/Ollama
        print("[LLM Init] LLM initialization complete.")

    def __config(self, config=None):
        print("[LLM Config] Configuring LLM parameters (mainly for OpenAI/Ollama)...")
        if config is None:
            print("[LLM Config] Using default config.")
            config = LLM.llm_default_config
        else:
            print("[LLM Config] Using provided config.")

        #Assign new config
        self.max_tokens        = config.get('max_tokens', LLM.llm_default_config['max_tokens'])
        self.temperature       = config.get('temperature', LLM.llm_default_config['temperature'])
        self.top_p             = config.get('top_p', LLM.llm_default_config['top_p'])
        self.stop              = config.get('stop', LLM.llm_default_config['stop'])
        self.seed              = config.get('seed', LLM.llm_default_config['seed'])
        print(f"[LLM Config] OpenAI/Ollama params: temp={self.temperature}, max_tokens={self.max_tokens}, top_p={self.top_p}, seed={self.seed}, stop={self.stop}")

    def query(self, prompt, end_when_error=False, max_retry=2) -> tuple:
        from datetime import datetime
        print(f"\n[LLM Query] Received query for API_TYPE: {self.API_TYPE}")

        #Extern messages queue for OpenAI and Ollama:
        messages_for_api_call = []
        if self.API_TYPE == "azure_openai" or self.API_TYPE == "ollama":
            messages_for_api_call.extend(self.initial_examples)
            messages_for_api_call.append({"role": "user", "content": prompt})
            print(f"[LLM Query] Messages for OpenAI/Ollama (count={len(messages_for_api_call)}):")
            for i, msg in enumerate(messages_for_api_call[-3:]):
                print(f"  Role: {msg.get('role')}, Content: {str(msg.get('content', ''))[:100]}...")

        # Salvataggio Query (invariato concettualmente, ma il contenuto di messages_for_api_call cambia)
        # ... (codice di salvataggio query) ...

        conn_success, llm_output = False, ""
        n_retry = 0
        while not conn_success and n_retry < max_retry:
            n_retry += 1
            print(f"[LLM Query] Attempt {n_retry}/{max_retry}...")
            try:
                if self.API_TYPE == "azure_openai":
                    llm_output = self.__connect_openai(messages_for_api_call)
                elif self.API_TYPE == "ollama":
                    llm_output = self.__connect_ollama(messages_for_api_call)
                elif self.API_TYPE == "local":
                    if not self.local_instance:
                        raise ConnectionError("[LLM Query] LocalModel instance not initialized.")
                    llm_output = self.local_instance.generate(prompt)
                else:
                    raise ValueError(f"API_TYPE non valido: {self.API_TYPE}.")
                conn_success = True
                print(f"[LLM Query] Attempt {n_retry} successful.")
            except Exception as e:
                print(f"[ERROR][LLM Query] Attempt {n_retry} failed: {e}")

        if conn_success and (self.API_TYPE == "azure_openai" or self.API_TYPE == "ollama"):
            pass

        print(f"[LLM Query] Returning success={conn_success}.")
        return conn_success, llm_output
    
    def __connect_openai(self, messages):
        from openai import AzureOpenAI
        print("[Connect OpenAI] Establishing connection...") 
        client = AzureOpenAI(
            api_key        = os.environ[self.API_KEY_NAME],
            azure_endpoint = self.ENDPOINT,
            api_version    = self.API_VERSION,
        )
        if not client: 
             raise ConnectionError("[Connect OpenAI] Failed to create AzureOpenAI client.")
        print("[Connect OpenAI] Client created. Sending request...") 
        response = client.chat.completions.create(
            model             = self.engine,
            messages          = messages,
            temperature       = self.temperature,
            max_tokens        = self.max_tokens,
            top_p             = self.top_p,
            frequency_penalty = self.frequency_penalty,
            presence_penalty  = self.presence_penalty,
            seed              = self.seed,
            stop              = self.stop,
        )
        print("[Connect OpenAI] Request successful. Extracting content.") 
        output = response.choices[0].message.content
        print(f"[Connect OpenAI] Returning content (len={len(output)}).") 
        return output
 
    def __connect_ollama(self, messages):
        """Connette a Ollama API senza streaming, attendendo la risposta completa."""
        url = f"{self.BASE_URL}/api/chat"
        headers = {"Content-Type": "application/json"}

        options = {
            "temperature": self.temperature,
            "top_p": self.top_p,
            "seed": self.seed,
            "num_predict": self.max_tokens if self.max_tokens is not None and self.max_tokens > 0 else -1, 
            "stop": [self.stop] if self.stop else None,
        }
        options = {k: v for k, v in options.items() if v is not None} 

        data = {
            "model": self.MODEL_NAME,
            "messages": messages,
            "stream": False,
            "options": options
        }

        llm_output = ""
        response = None

        print(f"[Connect Ollama] Preparing request for URL: {url}")

        try:
            print("[Connect Ollama] Sending POST request and waiting for full response...")
            start_time = time.time()
            response = requests.post(url, headers=headers, json=data, timeout=3000) # Timeout 50 min for long answers
            duration = time.time() - start_time
            print(f"[Connect Ollama] Request sent. Status Code: {response.status_code}. Time taken: {duration:.2f}s") 

            response.raise_for_status() 

            print("[Connect Ollama] Status OK. Parsing full JSON response...")

            # Parsing
            try:
                full_response_json = response.json()
                message_data = full_response_json.get('message')
                if message_data and isinstance(message_data, dict):
                    llm_output = message_data.get('content', '')
                else:
                    print("[WARNING][Connect Ollama] 'message' key missing or not a dictionary in response.")
                    llm_output = "" 

                final_stats = {k: v for k, v in full_response_json.items() if k not in ['message', 'model', 'created_at']}
                if final_stats:
                    print(f"[Connect Ollama] Response stats: {final_stats}")

            except json.JSONDecodeError as e:
                print(f"\n[ERROR][Connect Ollama] Failed to parse JSON response: {e}")
                print(f"Response text was: {response.text}")
                raise 
            except KeyError as e:
                 print(f"\n[ERROR][Connect Ollama] Missing expected key in JSON response: {e}")
                 print(f"Received JSON: {full_response_json}") 
                 raise 

            if not llm_output:
                 print("[WARNING][Connect Ollama] Successfully received response, but extracted content is empty.")

            print(f"[Connect Ollama] Returning final output (len={len(llm_output)}).") 
            return llm_output

        except requests.exceptions.RequestException as e:
            print(f"\n[FATAL ERROR][Connect Ollama] Request Exception: {e}") 
            if e.response is not None:
                print(f"Ollama Response Status Code: {e.response.status_code}")
                try:
                    error_body = e.response.text
                    print(f"Ollama Response Body:\n---\n{error_body}\n---")
                except Exception as read_err:
                    print(f"Could not read error response body: {read_err}")
            raise Exception(f"Errore nella richiesta a Ollama: {e}")

        except Exception as e:
             print(f"\n[FATAL ERROR][Connect Ollama] Unexpected error: {e}")
             print(traceback.format_exc())
             raise

        finally:
            if response is not None:
                try:
                    response.close()
                except Exception as close_err:
                    print(f"[ERROR][Connect Ollama] Error closing response connection: {close_err}")

    def clear_history(self): # Clear history
        if self.local_instance:
            self.local_instance.clear_history()
            print("[LLM] LocalModel history cleared.")
        else:
            print("[LLM] No LocalModel instance to clear history from.")