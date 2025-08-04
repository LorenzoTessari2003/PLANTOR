Generated markdown
# README Progetto PLANTOR - Riepilogo Attività

Questo documento descrive le principali modifiche e i contributi implementati di recente nel progetto PLANTOR. L'obiettivo di questi aggiornamenti è stato quello di potenziare, rendere più flessibile e affidabile il processo di traduzione da linguaggio naturale a codice Prolog, fondamentale per la pianificazione delle attività dei robot.

## Sommario delle Modifiche

Il lavoro si è concentrato su cinque aree principali, trasformando il sistema da un prototipo con componenti hard-coded a un'architettura più robusta e versatile:

1.  **Creazione di Script di Fine-Tuning per LLM:** Sviluppati due script per specializzare modelli linguistici open-source sul dominio specifico della traduzione da linguaggio naturale a Prolog per la pianificazione.
2.  **Sviluppo di un Dataset Strutturato:** Creato un dataset completo e ben organizzato per l'addestramento e la validazione dei modelli, coprendo tutti gli aspetti della generazione della Knowledge Base (KB), sia a basso che ad alto livello.
3.  **Integrazione di un Sistema Modulare per la Gestione dei Modelli:** Rifattorizzata la classe `LLM.py` per supportare in modo intercambiabile diverse tipologie di modelli (via API come Azure OpenAI, via servizi locali come Ollama, e modelli caricati direttamente in memoria).
4.  **Implementazione del Supporto per Modelli Locali:** Creata una classe riutilizzabile per caricare ed eseguire modelli fine-tuned (sia completi che quantizzati con QLoRA) direttamente in locale, garantendo indipendenza da servizi esterni e maggiore controllo.
5.  **Miglioramento del "Prompt Engineering":** Raffinati i prompt inviati al modello per guidarlo in modo più stringente, riducendo drasticamente gli errori di formattazione e aumentando l'affidabilità dell'output.

---

## Dettaglio dei Contributi

### 1. Fine-Tuning dei Modelli Linguistici

Per migliorare l'accuratezza del modello nel nostro dominio specifico, sono stati creati due script all'interno della cartella `model_training/` che implementano due diverse strategie di fine-tuning.

-   **`model_fine_tuning_complete.py`**: Esegue un fine-tuning completo (Full Fine-Tuning) del modello. Questo approccio modifica tutti i pesi del modello, garantendo un apprendimento profondo ma richiedendo risorse computazionali significative (VRAM > 24GB).
-   **`model_fine_tuning_qlora.py`**: Utilizza la tecnica QLoRA (Quantized Low-Rank Adaptation) per un fine-tuning più efficiente in termini di memoria. Invece di modificare l'intero modello, addestra solo un piccolo numero di "adattatori". Questo permette di ottenere ottimi risultati con risorse hardware più contenute (VRAM < 16GB).

Entrambi gli script sono configurati per utilizzare il modello `Qwen/Qwen2.5-Coder-0.5B-Instruct` come base, scelto per le sue buone capacità di generazione di codice, e sfruttano le librerie `transformers`, `peft` e `trl` di Hugging Face per l'intero processo.

### 2. Creazione del Dataset di Addestramento

Il successo del fine-tuning dipende da un dataset di alta qualità. È stata creata una struttura di dati completa all'interno delle cartelle `model_training/hl/` e `model_training/ll/`.

-   **Struttura:** Il dataset è composto da file Python (es. `hl_actions_examples.py`, `ll_kb_examples.py`) che contengono liste di dizionari. Ogni dizionario rappresenta un esempio di addestramento con due chiavi:
    -   `natural_language`: Una descrizione in linguaggio naturale di uno scenario o di un'azione.
    -   `prolog_code`: Il corrispondente codice Prolog che il modello deve imparare a generare.
-   **Copertura:** Il dataset copre sistematicamente tutte le fasi di generazione della KB:
    -   **High-Level (HL):** Knowledge Base, Stati Iniziali/Finali, Azioni.
    -   **Low-Level (LL):** Knowledge Base, Stati Iniziali/Finali, Azioni e i Mappings tra azioni HL e LL.

Questo dataset aggregato viene utilizzato dagli script di fine-tuning per specializzare il modello.

### 3. Architettura Modulare dei Modelli (`LLM.py`)

Il file `LLM.py` è stato profondamente modificato per passare da una gestione monolitica a un'architettura modulare. Ora è possibile cambiare il modello utilizzato semplicemente modificando un file di configurazione, senza toccare il codice.

-   **Selezione tramite `API_TYPE`:** La logica si basa sul parametro `API_TYPE` definito nei file di configurazione YAML (in `LLM/conf/`). A seconda del valore, `LLM.py` instrada le richieste al metodo di connessione appropriato:
    -   `azure_openai`: Utilizza l'SDK di OpenAI per connettersi ai servizi di Azure.
    -   `ollama`: Invia richieste HTTP dirette a un'istanza locale di Ollama (come descritto nell'appendice).
    -   `local_qwen`: Istanzia e utilizza un modello caricato direttamente in memoria tramite la classe `LocalQwenModel`.

Questa flessibilità permette di testare e deployare rapidamente diversi modelli, da quelli cloud a quelli completamente offline.

### 4. Supporto per Modelli Locali (`Local_qwen.py`)

Per garantire la massima portabilità e la possibilità di operare senza connessione a internet, è stato creato il file `Local_qwen.py`.

-   **Classe `LocalQwenModel`:** Questa classe agisce come un wrapper per qualsiasi modello basato su architettura `transformers` salvato in locale.
-   **Caratteristiche:**
    -   **Supporto duale:** È in grado di caricare sia un modello fine-tuned completo sia un modello QLoRA (caricando il modello base e applicandovi sopra gli adattatori PEFT).
    -   **Gestione del prompt:** Formatta automaticamente i prompt secondo la sintassi richiesta dal modello (in questo caso, ChatML per Qwen).
    -   **Gestione della cronologia:** Mantiene una cronologia della conversazione per contestualizzare le risposte successive.
    -   **Configurabilità:** I parametri di generazione come `temperature`, `max_new_tokens`, etc., possono essere impostati dal file di configurazione YAML.

Questa classe è progettata per essere facilmente adattabile ad altri modelli locali con poche modifiche.

### 5. Miglioramento della Guida del Modello (Prompt Engineering)

Un intervento cruciale è stato il rafforzamento dei "system prompt" nei file `LLM.py` e, soprattutto, `ui_multi_steps.py`. Le versioni precedenti lasciavano troppa libertà interpretativa al modello, causando output non validi o con formattazione errata.

-   **Istruzioni Chiare e Vincolanti:** I prompt sono stati arricchiti con istruzioni esplicite e direttive critiche. Ad esempio:
    -   `"**CRITICAL: Use the tag ```ll_actions``` and NOT ```actions``` or any other tag.**"`
    -   `"**Wrap the COMPLETE updated knowledge base ... within Markdown tags ```kb ... ```.**"`
    -   `"**Do NOT include initial state, goal state, knowledge base, or any other information.**"`
-   **Logica di Controllo:** In `ui_multi_steps.py` è stata aggiunta una logica di validazione che controlla se il modello ha effettivamente prodotto i tag richiesti (es. `ll_actions`, `mappings`). In caso di fallimento, il sistema solleva un errore, impedendo la propagazione di dati malformati.

Queste modifiche hanno reso il processo di generazione del codice significativamente più stabile e prevedibile, riducendo la necessità di post-elaborazione e parsing complessi.