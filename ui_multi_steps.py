# This file is the main file which coordinates the different aspects of the planner. 

import re
import os, sys
from pathlib import Path

from python_interface import planner
from LLM.LLM import LLM
from python_interface.utility.utility import INFO, MSG, FAIL

# Model path
# LLM_CONF_PATH    = os.path.join(os.path.dirname(__file__), 'LLM', 'conf/gpt4o.yaml')
# LLM_CONF_PATH    = os.path.join(os.path.dirname(__file__), 'LLM', 'conf/gpt4o-mini-fine-tuned.yaml')
# LLM_CONF_PATH    = os.path.join(os.path.dirname(__file__), 'LLM', 'conf/gpt4o-fine-tuned.yaml')
# LLM_CONF_PATH    = os.path.join(os.path.dirname(__file__), 'LLM', 'conf/gpt40-128k.yaml')
# LLM_CONF_PATH    = os.path.join(os.path.dirname(__file__), 'LLM', 'conf/gpt35-turbo.yaml')
# LLM_CONF_PATH    = os.path.join(os.path.dirname(__file__), 'LLM', 'conf/gpt40-32k.yaml')
# LLM_CONF_PATH    = os.path.join(os.path.dirname(__file__), 'LLM', 'conf/llama2.yaml')
# LLM_CONF_PATH    = os.path.join(os.path.dirname(__file__), 'LLM', 'conf/llama3.2.yaml')
# LLM_CONF_PATH    = os.path.join(os.path.dirname(__file__), 'LLM', 'conf/deepseek-r1-7b.yaml')
# LLM_CONF_PATH    = os.path.join(os.path.dirname(__file__), 'LLM', 'conf/gemma3.yaml')
# LLM_CONF_PATH    = os.path.join(os.path.dirname(__file__), 'LLM', 'conf/qwen2.5.yaml') #it works
# LLM_CONF_PATH    = os.path.join(os.path.dirname(__file__), 'LLM', 'conf/local-qwen-0.5B.yaml')
# LLM_CONF_PATH    = os.path.join(os.path.dirname(__file__), 'LLM', 'conf/local-gemma3-0.3B_qlora.yaml')
# LLM_CONF_PATH    = os.path.join(os.path.dirname(__file__), 'LLM', 'conf/local-gemma3-0.3B.yaml')
LLM_CONF_PATH    = os.path.join(os.path.dirname(__file__), 'LLM', 'conf/local-qwen3-8B_qlora.yaml')


# Examples Path
EXAMPLES_PATH               = os.path.join(os.path.dirname(__file__), 'LLM', 'examples')
FORMAT_EXAMPLES_HL_PATH     = os.path.join(EXAMPLES_PATH, 'format', 'hl')
FORMAT_EXAMPLES_LL_PATH     = os.path.join(EXAMPLES_PATH, 'format', 'll')
FORMAT_EXAMPLES_HL_KB       = os.path.join(FORMAT_EXAMPLES_HL_PATH, 'kb_hl.yaml')
FORMAT_EXAMPLES_HL_STATES   = os.path.join(FORMAT_EXAMPLES_HL_PATH, 'states_hl.yaml')
FORMAT_EXAMPLES_HL_ACTIONS  = os.path.join(FORMAT_EXAMPLES_HL_PATH, 'actions_hl.yaml')
FORMAT_EXAMPLES_LL_KB       = os.path.join(FORMAT_EXAMPLES_LL_PATH, 'kb_ll.yaml')
FORMAT_EXAMPLES_LL_STATES   = os.path.join(FORMAT_EXAMPLES_LL_PATH, 'states_ll.yaml')
FORMAT_EXAMPLES_LL_ACTIONS  = os.path.join(FORMAT_EXAMPLES_LL_PATH, 'actions_ll.yaml')
FORMAT_EXAMPLES_LL_MAPPINGS = os.path.join(FORMAT_EXAMPLES_LL_PATH, 'mappings_ll.yaml')


# Output Path
OUTPUT_PATH = os.path.join(os.path.dirname(__file__), 'output')
os.makedirs(OUTPUT_PATH, exist_ok=True)
OUTPUT_KB_FILE = os.path.join(OUTPUT_PATH, 'kb_ll.pl')
OUTPUT_HL_KB_FILE = os.path.join(OUTPUT_PATH, 'kb_hl.pl')
OUTPUT_BT_FILE = os.path.join(OUTPUT_PATH, 'BT.xml')
OUTPUT_FILE_CC = os.path.join(OUTPUT_PATH, "output_cc.txt")
OUTPUT_FILE_HL = os.path.join(OUTPUT_PATH, "output_hl.txt")
OUTPUT_FILE_LL = os.path.join(OUTPUT_PATH, "output_ll.txt")

WAIT = False

## FUNCTIONS ###########################################################################################################

def scan_and_extract(kb, response):
    """
    :brief: This function scans the code produced by the LLM and extracts the different parts that are in the form of
            ```<tag> 
            <content>
            ```
            If content is not empty, it is added to the knowledge base, even if the key was already present.
    :param kb: Dictionary where the extracted information will be stored
    :param response: The response from the LLM
    :return: None
    """
    pattern = re.compile(r'\`\`\`\s*(\w+)\s*([^\`]*?)\`\`\`', re.DOTALL)
    matches = pattern.findall(response)
    for Key, value in matches:
        assert "prolog" not in Key.lower(), f"Prolog tags are not allowed. Found {Key}"
        key = Key.lower().replace(" ", "")
        value = value.strip()

        if value == "":
            continue

        kb[key] = value


########################################################################################################################

def hl_llm_multi_step(query, llm) -> dict:
    """
    :brief This function uses the LLM to extract the high-level knowledge base, the initial and final states.
    :param query: The query that will be used to extract the knowledge base, initial and final states. 
    :return: A tuple containing the knowledge base and the response from the LLM
    """
    file = open(OUTPUT_FILE_HL, "w+")
    kb = {}
    examples = ""
    LLM_WARNING = "You are an expert Prolog programming assistant. Your task is to ALWAYS output ONLY a Markdown code block with the tag needed. Do not write explanations, notes, or comments outside the block. Never output <think> or any other tags. Always use the correct tag."

    # --- 1. Generate Knowledge Base ---
    INFO("\r[HL] Generating knowledge base")
    llm.clear_history() # Clear messages
    examples = read_example_file(FORMAT_EXAMPLES_HL_KB)
    kb_query = (
        "You now have to produce code for the task that follows.\n" 
        f"\n{query}\n" +
        "\nWrite the static knowledge base. Remember to specify all the correct predicates and identify which are "
        "the predicates that are resources and to wrap it into Markdown tags \"```kb\" and NOT with \"```Prolog\" or other tags."
        " Use this format as response:\n"
        f"\n{examples}\n" +
        f"\n{LLM_WARNING}\n"
    )
    succ, tmp_response = llm.query(kb_query)
    assert succ, f"Failed to generate static knowledge base. Response: {tmp_response}"
    file.write(f"KB:\n{tmp_response}\n\n")
    scan_and_extract(kb, tmp_response)
    assert "kb" in kb, f"FATAL: 'kb' tag not found in HL response. Response was:\n{tmp_response}"

    # --- 2. Generate Init and Final State ---
    INFO("\r[HL] Generating initial and final states")
    llm.clear_history() # Clear history
    examples = read_example_file(FORMAT_EXAMPLES_HL_STATES)
    states_query = (
        "You now have to produce code for the task that follows.\n" 
        f"\n{query}\n" +
        f"\nGiven the following static knowledge base\n```kb\n{kb['kb']}\n```"
        "\nWrite the initial and final states, minding to include all the correct predicates. "
        "Remember to wrap it into Markdown tags \"```init\" and \"```goal\" and NOT with \"```prolog\" or other tags."
        " Use this format as response:\n"
        f"\n{examples}\n" +
        f"\n{LLM_WARNING}\n"
    )
    succ, tmp_response = llm.query(states_query)
    assert succ, f"Failed to generate initial and final states. Response: {tmp_response}"
    file.write(f"INIT/GOAL:\n{tmp_response}\n\n")
    scan_and_extract(kb, tmp_response)
    assert "init" in kb and "goal" in kb, f"FATAL: 'init' or 'goal' tag not found in HL response. Response was:\n{tmp_response}"

    # --- 3. Generate Actions ---
    INFO("\r[HL] Generating actions set")
    llm.clear_history() # Clear history
    examples = read_example_file(FORMAT_EXAMPLES_HL_ACTIONS)
    actions_query = (
        "You now have to produce code for the task that follows.\n" 
        f"\n{query}\n" +
        f"Given the following static knowledge base\n```kb\n{kb['kb']}\n```" +
        f"\nKnowing that the initial state is the following\n```init\n{kb['init']}\n```" +
        f"\nKnowing that the goal state is the following\n```goal\n{kb['goal']}\n```" +
        "\nWrite the set of temporal actions divided into _start and _end actions. "
        "Remember to wrap it into Markdown tags \"```actions\" and NOT with \"```prolog\" or other tags."
        " Use this format as response:\n"
        f"\n{examples}\n" +
        f"\n{LLM_WARNING}\n"
    )
    succ, response = llm.query(actions_query)
    assert succ, f"Failed to generate actions. Response: {response}"
    file.write(f"ACTIONS:\n{response}\n")
    scan_and_extract(kb, response)
    assert "actions" in kb, f"FATAL: 'actions' tag not found in HL response. Response was:\n{response}"

    write_to_file(kb, output_file=OUTPUT_HL_KB_FILE)
    file.close()
    return kb


########################################################################################################################


def ll_llm_multi_step(query, kb, llm) -> dict: 
    hl_kb = f"""
    ```kb
    {kb["kb"]}
    ```
    ```init
    {kb["init"]}
    ```
    ```goal
    {kb["goal"]}
    ```
    ```actions
    {kb["actions"]}
    ```"""

    file = open(OUTPUT_FILE_LL, "w+")
    LLM_WARNING = "You are an expert Prolog programming assistant. Your task is to ALWAYS output ONLY a Markdown code block with the tag needed. Do not write explanations, notes, or comments outside the block. Never output <think> or any other tags. Always use the correct tag."
    LLM_LL_WARNING = "Remember to prepend the low-level predicates with `ll_` and also to not use the high-level predicates inside the low-level actions as they may lead to errors."

    # --- 1. Generate LL KB ---
    INFO("\r[LL] Generating knowledge base")
    llm.clear_history()
    examples = read_example_file(FORMAT_EXAMPLES_LL_KB)
    kb_query = (
        "You now have to produce code for the task that follows.\n" 
        f"\n{query}\n" +
        "Given the following high-level knowledge-base:\n{}\n".format(kb['kb']) + 
        "\n**Your task is to update the general knowledge base to include the new low-level predicates and resources.**" +
        f"\n**Wrap the COMPLETE updated knowledge base (including blocks, agents, positions, ll_predicates, etc.) within Markdown tags ```kb ... ```.**" + 
        f"\n**CRITICAL: Use the tag ```kb``` for the entire updated knowledge base.**" + 
        " Use this format as response:\n"
        f"\n{examples}\n" +
        f"\n{LLM_LL_WARNING}\n" + f"\n{LLM_WARNING}\n"
        "\nNow, generate the updated low-level knowledge base for the current problem:"
    )
    succ, response = llm.query(kb_query)
    assert succ, f"Failed to generate LL KB. Response: {response}"
    file.write(f"KB: {response}\n\n")
    scan_and_extract(kb, response)
    assert "kb" in kb, f"FATAL: 'kb' tag not found in LL response. Response was:\n{response}"

    # --- 2. Generate LL States ---
    INFO("\r[LL] Generating initial and final states")
    llm.clear_history()
    examples = read_example_file(FORMAT_EXAMPLES_LL_STATES)
    states_query = (
        "You now have to produce code for the task that follows.\n" 
        f"\n{query}\n" +
        "\nGiven the complete low-level knowledge-base generated previously:\n```kb\n{}\n```\n".format(kb["kb"]) + 
        "And given the high-level initial and final states as reference:\n```init\n{}\n```\n```goal\n{}\n```\n".format(kb["init"], kb["goal"]) + 
        "\n**Your task is to update ONLY the initial and final states to reflect the low-level details.**" +
        "\n**Wrap the updated initial state within Markdown tags ```init ... ```.**" + 
        "\n**Wrap the updated final state within Markdown tags ```goal ... ```.**" + 
        "\n**CRITICAL: Output ONLY these two blocks (`init` and `goal`).**" + 
        "\n**Do NOT include the knowledge base (`kb`), actions (`ll_actions`), resources, or any other information.**" +
        f"\n**Do NOT use ```python``` or any other tags besides ```init``` and ```goal```.**" + 
        " Use this format as response:\n"
        f"\n{examples}\n" +
        f"\n{LLM_LL_WARNING}\n" + f"\n{LLM_WARNING}\n"
        "\nNow, generate the updated low-level initial and final state for the current problem:"
    )
    succ, response = llm.query(states_query)
    assert succ, f"Failed to query LLM for LL init/goal states. Response: {response}"
    file.write(f"INIT/GOAL: {response}\n\n")
    scan_and_extract(kb, response)
    assert "init" in kb and "goal" in kb, f"FATAL: 'init' or 'goal' tag not found in LL response. Response was:\n{response}"
    
    # --- 3. Generate LL Actions ---
    INFO("\r[LL] Generating actions set")
    llm.clear_history()
    examples = read_example_file(FORMAT_EXAMPLES_LL_ACTIONS)
    ll_actions_query = (
        "You now have to produce code for the task that follows.\n" 
        f"\n{query}\n" +
        "Given the following high-level knowledge-base:\n{}\n".format(hl_kb) +
        "Given the refactored low-level knowledge-base:\n```kb\n{}\n```\n".format(kb["kb"]) +
        "\nGiven the low-level initial state:\n```init\n{}\n```".format(kb["init"]) + 
        "\nAnd the low-level goal state:\n```goal\n{}\n```".format(kb["goal"]) +
        "\n**Your task is to generate ONLY the set of low-level actions.**" +
        "\n**Wrap the low-level actions within Markdown tags ```ll_actions ... ```.**" + 
        "\n**CRITICAL: Use the tag ```ll_actions``` and NOT ```actions``` or any other tag.**" + 
        "\n**Do NOT include initial state, goal state, knowledge base, or any other information.**" +
        "\n**Just provide the list of low-level actions inside the ```ll_actions``` tag.**" + 
        " Use this format as response:\n"
        f"\n{examples}\n" +
        f"\n{LLM_LL_WARNING}\n" + f"\n{LLM_WARNING}\n"
        "\nNow, generate the specific low-level actions for the current problem:" 
    )
    succ, response = llm.query(ll_actions_query)
    assert succ, f"Failed to query LLM for LL actions. Response: {response}"
    file.write(f"ACTIONS: {response}\n\n")
    scan_and_extract(kb, response)
    assert "ll_actions" in kb, f"FATAL: 'll_actions' tag not found in LL response. Response was:\n{response}"

    # --- 4. Generate Mappings ---
    INFO("\r[LL] Generating mappings")
    llm.clear_history()
    examples = read_example_file(FORMAT_EXAMPLES_LL_MAPPINGS)
    mappings_query = (
        "You now have to produce code for the task that follows.\n" 
        f"\n{query}\n" +
        "Given the following high-level knowledge-base:\n{}\n".format(hl_kb) + \
        "Given the refactored low-level knowledge-base:\n```kb\n{}\n```\n".format(kb["kb"]) + \
        "Given the initial state:\n```init\n{}\n```\n".format(kb["init"]) + \
        "Given the final state:\n```goal\n{}\n```\n".format(kb["goal"]) + \
        "Given the low-level actions set:\n```actions\n{}\n```\n".format(kb["ll_actions"]) + \
        "\n**Provide the mappings from high-level actions to low-level actions.**" +
        "\n**Remember that the mappings are only for the start actions.**" +
        "\n**Wrap the mappings within Markdown tags ```mappings ... ```.**" + 
        "\n**CRITICAL: Use the tag ```mappings``` and NOT ```kb``` or any other tag.**" + 
        " Use this format as response:\n"
        f"\n{examples}\n" +
        f"\n{LLM_LL_WARNING}\n" + f"\n{LLM_WARNING}\n"
        "\nNow, generate the mappings for the current problem:"
    )
    succ, response = llm.query(mappings_query)
    assert succ, f"Failed to generate LL Mappings. Response: {response}"
    file.write(f"MAPPINGS: {response}\n")
    scan_and_extract(kb, response)
    assert "mappings" in kb, f"FATAL: 'mappings' tag not found in LL response. Response was:\n{response}"

    file.close()
    write_to_file(kb, output_file=OUTPUT_KB_FILE)
    return kb
    

########################################################################################################################

def add_dynamic_from_resources(kb_file_path):
    """
    Legge un file KB Prolog.
    Trova i predicati definiti in `resources(PredicatoConArgomenti)`.
    Genera le direttive `:- dynamic nome/arità` ESCLUSIVAMENTE per questi predicati.
    Aggiunge queste direttive all'inizio del file, sovrascrivendolo.
    """
    predicate_signatures_from_resources = set()
    original_content = ""

    if not isinstance(kb_file_path, (str, bytes, os.PathLike)):
        print(f"ERRORE: kb_file_path deve essere una stringa di percorso, non {type(kb_file_path)}")
        return

    try:
        with open(kb_file_path, 'r', encoding='utf-8') as f:
            original_content = f.read()

        # Regex per trovare resources(NomePredicato(...argomenti...)).
        resource_pattern = re.compile(r"resources\(\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*\(([^)]*)\)\s*\)\.")
        
        for match in resource_pattern.finditer(original_content):
            predicate_name = match.group(1)
            args_str = match.group(2).strip()
            
            # Stima l'arità
            if not args_str or args_str == '_': 
                arity = 1 
                if not args_str: 
                    arity = 0 
            else:
                arity = args_str.count(',') + 1
            
            predicate_signatures_from_resources.add(f"{predicate_name}/{arity}")

    except FileNotFoundError:
        print(f"ERRORE: File {kb_file_path} non trovato.")
        return
    except Exception as e:
        print(f"ERRORE durante la lettura o l'analisi del file per 'resources': {e}")
        return

    dynamic_directives_str = ""
    if predicate_signatures_from_resources: # Solo se abbiamo trovato qualcosa in resources/1
        dynamic_directives_str += "% Dynamic predicates from resources (auto-generated by Python script)\n"
        for sig in sorted(list(predicate_signatures_from_resources)):
            dynamic_directives_str += f":- dynamic {sig}.\n"
        dynamic_directives_str += "\n"
    else:
        print("No predicates found in 'resources(...)' per generare direttive dynamic.")

    # Rimuovi eventuali vecchie direttive dynamic auto-generate (con questo specifico commento)
    pattern_to_remove = re.compile(
        r"^% Dynamic predicates from resources \(auto-generated by Python script\)\s*\n"
        r"(?:P<directive>:- dynamic\s+.*?\.\s*\n)*\n?", 
        re.MULTILINE
    )
    content_without_old_directives = pattern_to_remove.sub("", original_content)
    content_without_old_directives = content_without_old_directives.lstrip()

    # Se non ci sono nuove direttive da aggiungere E non c'era un blocco da rimuovere,
    # potremmo evitare di riscrivere il file inutilmente.
    # Ma per semplicità, se dynamic_directives_str è vuoto (perché predicate_signatures_from_resources era vuoto)
    # e content_without_old_directives è uguale a original_content, potremmo saltare la scrittura.
    # Per ora, la logica è: se ci sono direttive da scrivere (nuove o perché un vecchio blocco è stato rimosso), scrivi.
    # Se predicate_signatures_from_resources è vuoto, dynamic_directives_str sarà vuoto.
    # Se non c'era un blocco da rimuovere, new_content sarà uguale a original_content.

    new_content = dynamic_directives_str + content_without_old_directives
    
    # Evita di riscrivere il file se il contenuto non è cambiato
    if new_content.strip() == original_content.strip() and not predicate_signatures_from_resources:
         print(f"Nessuna modifica necessaria per {kb_file_path} (nessun predicato in resources o contenuto identico).")
         return

    try:
        with open(kb_file_path, 'w', encoding='utf-8') as f:
            f.write(new_content)
        if predicate_signatures_from_resources:
            print(f"Aggiunte/Aggiornate direttive 'dynamic' (da resources) in {kb_file_path}")
        elif new_content.strip() != original_content.strip(): # Caso in cui abbiamo solo rimosso un vecchio blocco
            print(f"Rimosso vecchio blocco di direttive 'dynamic' da {kb_file_path}")
        # else: non dovrebbe arrivare qui se abbiamo gestito il caso "nessuna modifica"
            
    except Exception as e:
        print(f"ERRORE durante la scrittura del file: {e}")

########################################################################################################################

def find_plan(kb_file = OUTPUT_KB_FILE, xml_file = OUTPUT_BT_FILE, stn_file = "", html_file = "") -> planner.BehaviourTree:
    # Find plan
    INFO(f"Finding plan for kb in {kb_file}")

    if not os.path.exists(kb_file):
        raise FileNotFoundError(f"Knowledge base file not found at {kb_file}")
    INFO(F"Path: {kb_file}")
    data_dict = planner.PrologLib.execTest(kb_path=kb_file)

    INFO("Optimizing")
    milp_solver = planner.MILPSolver(
        data_dict["tt_actions"],
        data_dict["actions"],
        data_dict["adj_matrix"],
        data_dict["resources"],
        data_dict["resources_list"],
        data_dict["ll_actions_list"]
    )

    milp_solver.solve()

    if stn_file != "":
        milp_solver.draw_graph_from_matrix(stn_file, open_browser=False)


    INFO("Extracing BT")
    bt = milp_solver.extract_BT()
    
    if html_file!="":
        bt.draw(html_file)

    bt.toXML(xml_file)
    INFO(f"Done extracting BT to {xml_file}")

    return bt
    

########################################################################################################################


def execute_plan(plan_file):
    # Execute plan
    INFO("Executing plan")


########################################################################################################################


def write_to_file(kb, output_file = OUTPUT_KB_FILE):
    INFO(f"Writing knowledge base to {output_file}")
    os.makedirs(Path(output_file).parent, exist_ok=True)
    with open(output_file, "w") as file:
        file.write("% This file was automatically generated by the LLM system\n")
        first_line = True
        for key, value in kb.items():
            if not first_line:
                file.write("\n")
            file.write(f"%%%%%%%%%%%%%%%%%%%%%%%\n% {key}\n%%%%%%%%%%%%%%%%%%%%%%%\n{value}\n")
            first_line = False
#         actions_path = os.path.join(os.path.dirname(__file__), 'prolog_planner', 'examples', 'blocks_world', 'actions.pl')
#         mappings_path = os.path.join(os.path.dirname(__file__), 'prolog_planner', 'examples', 'blocks_world', 'mappings.pl')
#         file.write(f"""
# % :- ensure_loaded('{actions_path}').
# % :- ensure_loaded('{mappings_path}').
# """)


########################################################################################################################


def read_from_file():
    kb = dict()
    with open(OUTPUT_KB_FILE, "r") as file:
        new_key = False
        new_value = False
        value = ""
        for line in file:
            if new_key and line == "%%%%%%%%%%%%%%%%%%%%%%%\n":
                new_key = False

            elif new_key:
                key = line.split('%')[1].strip()
                new_value = True
                kb[key] = ""

            elif not new_key and line == "%%%%%%%%%%%%%%%%%%%%%%%\n":
                new_value = False
                if value != "":
                    kb[key] = value
                value = ""
                new_key = True

            elif not new_key and new_value:
                value += line

        kb[key] = value

    return kb


########################################################################################################################

def read_example_file(file_path: str) -> str:
    if not os.path.exists(file_path):
        FAIL(f"Example file not found at {file_path}. Cannot proceed.")
        sys.exit(1)
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            return f.read()
    except Exception as e:
        FAIL(f"Error reading example file {file_path}: {e}")
        sys.exit(1)

########################################################################################################################

def main():
    assert os.path.exists(LLM_CONF_PATH), f"LLM configuration file not found at {LLM_CONF_PATH}"
    assert os.path.exists(FORMAT_EXAMPLES_HL_PATH), f"Format high-level examples path not found at {FORMAT_EXAMPLES_HL_PATH}"
    assert os.path.exists(FORMAT_EXAMPLES_LL_PATH), f"Format low-level examples path not found at {FORMAT_EXAMPLES_LL_PATH}"
    # print(HL_EXAMPLES_CONFIG_PATH)

    query_hl = ""
    query_ll = ""

    GENERAL_DIR = os.path.join(os.path.dirname(__file__), 'exps', 'multi-steps', 'blocks_world', '1')
    assert os.path.exists(GENERAL_DIR), f"General directory not found at {GENERAL_DIR}"
    assert os.path.exists(os.path.join(GENERAL_DIR, 'hl_query.txt')), f"High-level query file not found at {os.path.join(GENERAL_DIR, 'hl_query.txt')}"
    assert os.path.exists(os.path.join(GENERAL_DIR, 'll_query.txt')), f"Low-level query file not found at {os.path.join(GENERAL_DIR, 'll_query.txt')}"
    
    with open(os.path.join(GENERAL_DIR, 'hl_query.txt'), 'r') as file:
        query_hl = file.read()
    query_hl+="\nRemember that the tags are in the Markdown form of ```tag and not <tag>"
    with open(os.path.join(GENERAL_DIR, 'll_query.txt'), 'r') as file:
        query_ll = file.read()
    query_ll+="\nRemember that the tags are in the Markdown form of ```tag and not <tag> and that the low-level actions have the tag ll_actions and not actions"

    '''
    compr, resp = llm_scenario_comprehension(query_hl, query_ll)
    if not compr:
        FAIL(f"There was a problem with the comprehension of the scenario {resp}")
    
    if WAIT:
        input("Consistency check finished, press enter to continue...")
    '''

    # Init LLM
    INFO("\r[Main] Initializing the Language Model...", imp=True)
    llm = LLM(llm_connection_config_file=LLM_CONF_PATH)
    INFO("\r[Main] Language Model Initialized.", imp=True)

    # Use HL examples LLM to extract KB
    #llm.initial_examples = llm.load_examples([HL_EXAMPLES_CONFIG_PATH])
    hl_kb = hl_llm_multi_step(query_hl, llm)
    write_to_file(hl_kb)

    if WAIT:
        input("HL finished, press enter to continue...")
    hl_kb = read_from_file()

    # Use LL examples LLM to extract KB
    #llm.initial_examples = llm.load_examples([LL_EXAMPLES_CONFIG_PATH])
    kb = ll_llm_multi_step(query_ll, hl_kb, llm)
    write_to_file(kb)
    add_dynamic_from_resources(OUTPUT_KB_FILE)

    if WAIT:
        input("LL finished, press enter to continue...")
    INFO("LLM PART DONE!")

    INFO(f"\nReading final knowledge base from file: {OUTPUT_KB_FILE}")
    try:
        kb_from_file = read_from_file()
        INFO("Knowledge base read successfully from file.")
    except FileNotFoundError:
        FAIL(f"ERROR: Knowledge base file not found at {OUTPUT_KB_FILE}. Cannot proceed.")
        sys.exit(1)
    except Exception as e:
        FAIL(f"ERROR: Failed to read or parse knowledge base file {OUTPUT_KB_FILE}: {e}")
        sys.exit(1)

    # Print kb
    '''
    INFO("\nPrinting Contents of the Knowledge Base Read from File:")
    print("=" * 80) 
    preferred_order = ["kb", "init", "goal", "actions", "ll_actions", "mappings"] 
    printed_keys = set()

    for key in preferred_order:
        if key in kb_from_file:
            value = kb_from_file[key]
            print(f"### Sezione: {key.upper()} ###") # Titolo della sezione
            print(f"--- Contenuto per la chiave: '{key}' ---")
            content_to_print = value.strip() if isinstance(value, str) else str(value)
            if not content_to_print:
                print("[CONTENUTO VUOTO o NON STRINGA]")
            else:
                print("```")
                print(content_to_print)
                print("```")
            print("-" * 80) # Separatore tra sezioni
            printed_keys.add(key) # Segna come stampata

    # Unexpected keys
    remaining_keys = kb_from_file.keys() - printed_keys
    if remaining_keys:
        print("### Sezioni Aggiuntive/Inaspettate nel File ###")
        for key in remaining_keys:
            value = kb_from_file[key]
            print(f"--- Contenuto per la chiave: '{key}' ---")
            content_to_print = value.strip() if isinstance(value, str) else str(value)
            if not content_to_print:
                print("[CONTENUTO VUOTO o NON STRINGA]")
            else:
                print("```")
                print(content_to_print)
                print("```")
            print("-" * 80)

    print("=" * 80)
    INFO("End of printing knowledge base from file.")
    '''

    # Planning phase
    INFO("\nProceeding with Plan Finding...")
    try:
        bt = find_plan(kb_file=OUTPUT_KB_FILE)
        INFO("Plan finding completed.")

        execute_plan(bt)
        INFO("Plan execution completed.")

    except FileNotFoundError as e:
         FAIL(f"ERROR during planning: {e}")
         sys.exit(1)
    except Exception as e:
         FAIL(f"ERROR during planning or execution: {e}")
         # Considera di stampare il traceback per errori inaspettati
         import traceback
         traceback.print_exc()
         sys.exit(1)

    INFO("\nALL DONE!")

if __name__ == "__main__":
    main()