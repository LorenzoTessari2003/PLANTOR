# PLANTOR - Planning With Natural Language for Task-Oriented Robots

## Prerequisiti

*   **Ollama:** È necessario installare Ollama per scaricare ed eseguire i modelli LLM richiesti.

## Installazione di Ollama

Ollama permette di eseguire modelli linguistici di grandi dimensioni localmente. Segui le istruzioni per il tuo sistema operativo per installarlo:

### Windows

Ollama per Windows è attualmente in fase di anteprima.

1.  **Scarica:** Vai su [https://ollama.com/download](https://ollama.com/download) e scarica l'installer per Windows.
2.  **Installa:** Esegui il file `.exe` scaricato e segui le istruzioni di installazione.
3.  **Verifica (PowerShell/Prompt dei Comandi):** Apri PowerShell o il Prompt dei Comandi e digita:
    ```bash
    ollama --version
    ```
    Consulta la [documentazione ufficiale di Ollama per Windows](https://github.com/ollama/ollama/blob/main/docs/windows.md) per ulteriori dettagli o risoluzione problemi.

    ### macOS

1.  **Scarica:** Vai su [https://ollama.com/download](https://ollama.com/download) e scarica l'applicazione per macOS.
2.  **Installa:** Apri il file `.zip` scaricato e trascina `Ollama.app` nella cartella Applicazioni.
3.  **Avvia:** Apri l'applicazione Ollama. Verrà eseguita in background (cerca l'icona nella barra dei menu).
4.  **Verifica (Terminale):** Apri il Terminale e digita `ollama --version`. Dovresti vedere la versione installata.

### Linux

1.  **Installa tramite Script:** Apri il tuo terminale ed esegui il comando seguente:
    ```bash
    curl -fsSL https://ollama.com/install.sh | sh
    ```
    Questo script scaricherà ed installerà Ollama. Potrebbe richiedere la password di amministratore (`sudo`).
2.  **Verifica (Terminale):** Dopo l'installazione, digita nel terminale:
    ```bash
    ollama --version
    ```
    Dovresti vedere la versione installata.

## Download dei Modelli LLM Richiesti

PLANTOR richiede i seguenti modelli LLM specifici. Assicurati che Ollama sia in esecuzione (controlla l'icona nella barra dei menu su macOS/Windows o che il servizio sia attivo su Linux).

Apri il tuo terminale (o PowerShell/Prompt dei Comandi) ed esegui i seguenti comandi **uno alla volta** per scaricare ciascun modello:

1.  **Download `deepseek-r1:7b`:**
    ```bash
    ollama pull deepseek-r1:7b
    ```

2.  **Download `llama2`:**
    ```bash
    ollama pull llama2
    ```

3.  **Download `gemma3:12b`:**
    ```bash
    ollama pull gemma3:12b
    ```

**Spiegazione:**

*   `ollama pull <nome_modello>:<tag>` è il comando per scaricare un modello specifico dalla libreria Ollama.
*   Il download potrebbe richiedere tempo a seconda della dimensione del modello e della velocità della tua connessione internet. Ollama mostrerà lo stato del download nel terminale.

## Funzionamento Dettagliato di `__connect_ollama(self, messages)`

Questo metodo gestisce l'interazione con un'istanza **Ollama in esecuzione localmente** (o su un server raggiungibile specificato nel `BASE_URL`). È un'implementazione personalizzata che utilizza richieste HTTP dirette.

1.  **Comunicazione via API REST Locale:**
    *   A differenza di `__connect_openai` che usa un SDK specifico per un servizio cloud, `__connect_ollama` comunica direttamente con l'API REST esposta dal processo Ollama in esecuzione.
    *   Utilizza la libreria Python standard `requests` per inviare richieste HTTP POST.

2.  **Costruzione della Richiesta HTTP:**
    *   **URL:** L'URL di destinazione è costruito dinamicamente combinando il `BASE_URL` (es. `http://localhost:11434`) definito nel file di configurazione YAML con l'endpoint standard di Ollama per le chat: `/api/chat`.
    *   **Headers:** Imposta l'header `Content-Type` a `application/json`, indicando che il corpo della richiesta conterrà dati JSON.
    *   **Payload (Corpo JSON):** Viene creato un dizionario Python, poi convertito in JSON, contenente i dati necessari per Ollama:
        *   `model`: Specifica quale modello caricato in Ollama deve processare la richiesta. Il valore viene preso dal `MODEL_NAME` nel file di configurazione (es. `"llama2"`, `"gemma3:12b"`).
        *   `messages`: La lista completa dei messaggi della conversazione.
        *   `stream: False`: **Impostazione Cruciale.** Questo parametro dice a Ollama di **non inviare la risposta token per token (streaming)**, ma di **generare l'intera risposta e inviarla solo una volta completata**. Questo semplifica la gestione della risposta nel codice Python, ma significa che il programma attenderà finché l'intera risposta non sarà pronta.
        *   `options`: Un sotto-dizionario che mappa i parametri di generazione generici della classe `LLM` ai nomi specifici attesi da Ollama:
            *   `temperature`, `top_p`, `seed` vengono passati direttamente.
            *   `max_tokens` viene mappato a `num_predict` (impostato a -1 se `max_tokens` non è valido, che in Ollama di solito significa "nessun limite esplicito").
            *   `stop` viene passato come lista contenente la sequenza di stop.
            *   Vengono inclusi solo i parametri `options` che hanno un valore valido (non `None`).

3.  **Invio e Attesa della Risposta:**
    *   La richiesta POST viene inviata all'URL di Ollama usando `requests.post()`.
    *   Viene specificato un `timeout` elevato (es. 3000 secondi) per dare a Ollama tempo sufficiente per elaborare anche prompt complessi o generare risposte molto lunghe, senza che la richiesta scada prematuramente.
    *   Il codice attende passivamente la ricezione della risposta completa da Ollama.

4.  **Elaborazione della Risposta Completa:**
    *   **Controllo Status:** Verifica se lo status code HTTP della risposta è di successo (es. 200 OK). In caso di errore (es. 404 Not Found, 500 Internal Server Error), viene sollevata un'eccezione (`response.raise_for_status()`).
    *   **Parsing JSON:** Se lo status è OK, il corpo della risposta viene interpretato come JSON usando `response.json()`.
    *   **Estrazione Contenuto:** Il testo effettivo generato dall'LLM viene estratto dal percorso `['message']['content']` all'interno della struttura JSON della risposta di Ollama.
    *   **Gestione Errori:** Sono presenti blocchi `try...except` specifici per gestire errori comuni come problemi di rete (`requests.exceptions.RequestException`), errori durante il parsing del JSON (`json.JSONDecodeError`), o l'assenza di chiavi attese nella risposta (`KeyError`). Vengono stampati log dettagliati per aiutare nel debug.
    *   **Chiusura Connessione:** Viene assicurata la chiusura della connessione HTTP nel blocco `finally`.

5.  **Restituzione Risultato:** Ritorna la stringa di testo estratta (o una stringa vuota se l'estrazione fallisce o il contenuto è vuoto).

### Differenze Chiave rispetto a `__connect_openai`

| Caratteristica        | `__connect_ollama`                                  | `__connect_openai`                                    |
| :-------------------- | :-------------------------------------------------- | :---------------------------------------------------- |
| **Target Servizio**   | Istanza Ollama locale (o server specifico)          | Servizio Cloud Azure OpenAI                           |
| **Comunicazione**     | Richieste HTTP dirette (libreria `requests`)        | SDK Ufficiale (`openai` library)                      |
| **Autenticazione**    | Nessuna (implicita per connessione locale)          | API Key + Endpoint Azure                              |
| **Identificativo Modello** | `MODEL_NAME` (Tag modello Ollama)                | `engine` (Nome Deployment Azure)                      |
| **Struttura Chiamata** | `requests.post` a `/api/chat`                      | `client.chat.completions.create()`                    |
| **Payload / Parametri**| Payload JSON con sotto-dizionario `options`       | Parametri diretti al metodo SDK                       |
| **Mapping Parametri** | `max_tokens` -> `num_predict`                       | Mappatura diretta (gestita da SDK)                    |
| **Streaming**         | **Disabilitato esplicitamente** (`stream: False`)   | Gestito dall'SDK (ma non usato in questa implementazione) |
| **Struttura Risposta**| JSON con `['message']['content']`                   | Oggetto risposta SDK con `choices[0].message.content` |
| **Gestione Retry**    | Affidata al **loop esterno** nel metodo `query`      | Decorator `@retry` **interno** + loop esterno in `query` |

In sintesi, `__connect_ollama` offre un modo diretto e personalizzato per interagire con Ollama localmente tramite la sua API REST standard, attendendo la risposta completa. Al contrario, `__connect_openai` si affida all'SDK ufficiale per interfacciarsi con il servizio cloud Azure, astraendo molti dettagli della comunicazione HTTP sottostante ma richiedendo una configurazione specifica per l'autenticazione e l'identificazione del modello nel cloud. Entrambi i metodi sono progettati per restituire l'output testuale finale al metodo `query` per un'ulteriore elaborazione.