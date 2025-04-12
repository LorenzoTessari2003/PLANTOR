import yaml
import os
import requests
import json
from dotenv import load_dotenv
from retry import retry
import time 
import traceback 

try:
    import utility
except ImportError:
    from . import utility

class LLM:
    llm_default_config = {
        "max_tokens": 4096,
        "temperature": 0.0,
        "top_p": 0.0,
        "frequency_penalty": 0.0,
        "presence_penalty": 0.0,
        "stop": None,
        "seed": 42
    }

    def __init__(self, llm_connection_config_file = os.path.join(os.path.dirname(__file__), "conf/gemma3.yaml"), examples_yaml_file = [""], llm_config = None):
        print("[LLM Init] Starting LLM initialization...") 
        load_dotenv()

        self.messages = []
        self.system_msg = ""

        if len(examples_yaml_file) > 0 :
            print(f"[LLM Init] Loading examples from: {examples_yaml_file}") 
            initial_message_count = 0
            try:

                for file in examples_yaml_file:
                    if file == "" or not os.path.isfile(file):
                        print(f"[LLM Init] Skipping non-existent or empty example file: {file}")
                        continue
                    print(f"[LLM Init] Processing example file: {file}") 

                    tmp_messages = []
                    utility.includeYAML(file, tmp_messages, "") 
                    self.messages.extend(tmp_messages) 

                initial_message_count = len(self.messages)
                print(f"[LLM Init] Loaded {initial_message_count} initial messages from examples.") 
                if initial_message_count > 0:
                    print("[LLM Init] First few initial messages:")
                    for i, msg in enumerate(self.messages[:3]): # Stampa solo i primi 3 per brevità
                         print(f"  [{i}] Role: {msg.get('role')}, Content: {msg.get('content', '')[:100]}...") # Primi 100 char 
                    if initial_message_count > 3:
                        print("  ...")
                else:
                    print("[LLM Init] No initial messages loaded.") 

            except Exception as e:
                print(f"[ERROR][LLM Init] Failed to load examples: {e}")
                raise # Rilancia l'eccezione se il caricamento fallisce

        else:
            print("[LLM Init] No example files provided.")

        print("[LLM Init] Loading LLM connection config from: ", llm_connection_config_file)
        if llm_connection_config_file.endswith(".yaml") and os.path.isfile(llm_connection_config_file):
            with open(llm_connection_config_file) as file:
                llm_connection_config = yaml.load(file, Loader=yaml.FullLoader)

                self.engine  = llm_connection_config.get("LLM_VERSION", "N/A") 
                self.API_TYPE = llm_connection_config.get("API_TYPE", "azure_openai")
                self.BASE_URL = llm_connection_config.get("BASE_URL")
                self.MODEL_NAME = llm_connection_config.get("MODEL_NAME")

                print(f"[LLM Init] API_TYPE detected: {self.API_TYPE}") 
                if self.API_TYPE == "azure_openai":
                    self.API_KEY_NAME = llm_connection_config.get("API_KEY_NAME", "N/A")
                    self.ENDPOINT     = llm_connection_config.get("ENDPOINT", "N/A")
                    self.API_VERSION  = llm_connection_config.get("API_VERSION", "N/A")
                    print(f"[LLM Init] Azure Config - Engine: {self.engine}, Endpoint: {self.ENDPOINT}, API Version: {self.API_VERSION}") 
                elif self.API_TYPE == "ollama":
                    print(f"[LLM Init] Ollama Config - Base URL: {self.BASE_URL}, Model Name: {self.MODEL_NAME}, Engine (ignored?): {self.engine}") 
                    if not self.BASE_URL or not self.MODEL_NAME:
                         print("[WARNING][LLM Init] BASE_URL or MODEL_NAME missing for Ollama configuration!")
                else:
                    raise ValueError(f"API_TYPE non valido: {self.API_TYPE}. Deve essere 'azure_openai' o 'ollama'.")
        else:
            raise FileNotFoundError("The selected file {} does not exist or is not a yaml file".format(llm_connection_config_file))

        self.__config(llm_config)
        print("[LLM Init] LLM initialization complete.") 

    def __config(self, config = None):
        print("[LLM Config] Configuring LLM parameters...") 
        if config is None:
            print("[LLM Config] Using default config.") 
            config = LLM.llm_default_config
        else:
            print("[LLM Config] Using provided config.") 

            # Verifica chiavi mancanti rispetto al default
            missing_keys = [key for key in LLM.llm_default_config if key not in config]
            if missing_keys:
                print(f"[WARNING][LLM Config] Provided config is missing keys: {missing_keys}. Using defaults for them might occur implicitly if not set.")

        self.max_tokens        = config.get('max_tokens', LLM.llm_default_config['max_tokens'])
        self.temperature       = config.get('temperature', LLM.llm_default_config['temperature'])
        self.top_p             = config.get('top_p', LLM.llm_default_config['top_p'])
        self.frequency_penalty = config.get('frequency_penalty', LLM.llm_default_config['frequency_penalty'])
        self.presence_penalty  = config.get('presence_penalty', LLM.llm_default_config['presence_penalty'])
        self.stop              = config.get('stop', LLM.llm_default_config['stop'])
        self.seed              = config.get('seed', LLM.llm_default_config['seed'])
        print(f"[LLM Config] Final params: temp={self.temperature}, max_tokens={self.max_tokens}, top_p={self.top_p}, seed={self.seed}, stop={self.stop}") # DEBUG


    def query(self, prompt, end_when_error=False, max_retry=2) -> tuple:
        from datetime import datetime
        print(f"\n[LLM Query] Received query. Current message history length: {len(self.messages)}") 

        messages_for_this_query = list(self.messages)
        messages_for_this_query.append({"role": "user", "content": prompt})
        print(f"[LLM Query] Added user prompt. Total messages for this query: {len(messages_for_this_query)}") 

        # Salvataggio Query Inviata
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S_%f')
        output_dir = "output"
        os.makedirs(output_dir, exist_ok=True)
        sent_query_filename = os.path.join(output_dir, f"sent_query_{timestamp}.json")
        try:
            with open(sent_query_filename, "w", encoding='utf-8') as f:
                 json.dump(messages_for_this_query, f, indent=2, ensure_ascii=False)
            print(f"[LLM Query] Saved query being sent to: {sent_query_filename}") 
        except Exception as e:
            print(f"[ERROR][LLM Query] Failed to save sent query log: {e}")

        conn_success, llm_output = False, ""
        n_retry = 0
        while not conn_success and n_retry < max_retry:
            n_retry += 1
            print(f"[LLM Query] Attempt {n_retry}/{max_retry} to connect to LLM (API Type: {self.API_TYPE})...") # DEBUG
            try:
                if self.API_TYPE == "azure_openai":
                    llm_output = self.__connect_openai(messages_for_this_query)
                elif self.API_TYPE == "ollama":
                    llm_output = self.__connect_ollama(messages_for_this_query)
                else:
                    raise ValueError(f"API_TYPE non valido: {self.API_TYPE}.")

                conn_success = True
                print(f"[LLM Query] Attempt {n_retry} successful.") 

            except Exception as e:
                print(f"[ERROR][LLM Query] Attempt {n_retry} failed: {e}") 
                if end_when_error:
                    print("[LLM Query] Ending query due to error as requested by end_when_error=True.") 
                    break
                if n_retry < max_retry:
                    print(f"[LLM Query] Retrying after delay...") 
                else:
                    print(f"[LLM Query] Max retries reached.") 

        # Gestione Cronologia
        if conn_success:
            print("[LLM Query] Query successful. Adding assistant response to main history.") 
            assistant_message = {"role": "assistant", "content": llm_output}
            self.messages.append({"role": "user", "content": prompt}) # Aggiungi il prompt utente che ha funzionato
            self.messages.append(assistant_message) # Aggiungi la risposta
        else:
            print("[LLM Query] Query failed after all retries. Main history remains unchanged for this turn.") 

        print(f"[LLM Query] Returning success={conn_success}. Final history length: {len(self.messages)}") 
        
        # Stampa un pezzo dell'output per verifica rapida
        print(f"[LLM Query] Output snippet: {llm_output[:200] if llm_output else '<<EMPTY>>'}...") 
        return conn_success, llm_output
    
    @retry(tries=2, delay=30) 
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
            response = requests.post(url, headers=headers, json=data, timeout=3000) # Timeout 50 min per risposte lunghe
            duration = time.time() - start_time
            print(f"[Connect Ollama] Request sent. Status Code: {response.status_code}. Time taken: {duration:.2f}s") 

            response.raise_for_status() 

            print("[Connect Ollama] Status OK. Parsing full JSON response...")

            # Parsing risposta completa
            try:
                full_response_json = response.json()
                message_data = full_response_json.get('message')
                if message_data and isinstance(message_data, dict):
                    llm_output = message_data.get('content', '')
                else:
                    print("[WARNING][Connect Ollama] 'message' key missing or not a dictionary in response.")
                    llm_output = "" 

                # Log delle statistiche finali (se presenti e utili)
                final_stats = {k: v for k, v in full_response_json.items() if k not in ['message', 'model', 'created_at']}
                if final_stats:
                    print(f"[Connect Ollama] Response stats: {final_stats}")

            except json.JSONDecodeError as e:
                print(f"\n[ERROR][Connect Ollama] Failed to parse JSON response: {e}")
                print(f"Response text was: {response.text}") # Stampa il testo grezzo per debug
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
            # Rilancia l'eccezione affinché il ciclo di retry in query() possa gestirla
            raise Exception(f"Errore nella richiesta a Ollama: {e}")

        except Exception as e:
             print(f"\n[FATAL ERROR][Connect Ollama] Unexpected error: {e}")
             print(traceback.format_exc())
             raise # Rilancia per far fallire il tentativo

        finally:
            # Assicurati di chiudere la connessione se la risposta esiste
            if response is not None:
                try:
                    response.close()
                except Exception as close_err:
                    print(f"[ERROR][Connect Ollama] Error closing response connection: {close_err}")