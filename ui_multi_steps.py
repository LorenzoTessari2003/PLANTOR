# This file is the main file which coordinates the different aspects of the planner. 

import re
import os, sys
from pathlib import Path

from python_interface import planner
from LLM.LLM import LLM
from python_interface.utility.utility import INFO, MSG, FAIL


## GLOBAL VARIABLES ####################################################################################################

LLM_CONF_PATH    = os.path.join(os.path.dirname(__file__), 'LLM', 'conf/gpt4o.yaml')
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


EXAMPLES_PATH           = os.path.join(os.path.dirname(__file__), 'LLM', 'examples')
CC_EXAMPLES_CONFIG_PATH = os.path.join(EXAMPLES_PATH, 'cc', 'few-shots-cc.yaml')
CC_EXAMPLES_HL_PATH     = os.path.join(EXAMPLES_PATH, 'cc', 'hl.yaml')
CC_EXAMPLES_LL_PATH     = os.path.join(EXAMPLES_PATH, 'cc', 'll.yaml')
LL_EXAMPLES_CONFIG_PATH = os.path.join(EXAMPLES_PATH, 'multi', 'few-shots-ll.yaml')
HL_EXAMPLES_CONFIG_PATH = os.path.join(EXAMPLES_PATH, 'multi', 'few-shots-hl.yaml')

WAIT = False

OUTPUT_PATH = os.path.join(os.path.dirname(__file__), 'output')

os.makedirs(OUTPUT_PATH, exist_ok=True)

OUTPUT_KB_FILE = os.path.join(OUTPUT_PATH, 'kb_ll.pl')
OUTPUT_HL_KB_FILE = os.path.join(OUTPUT_PATH, 'kb_hl.pl')
OUTPUT_BT_FILE = os.path.join(OUTPUT_PATH, 'BT.xml')

OUTPUT_FILE_CC = os.path.join(OUTPUT_PATH, "output_cc.txt")
OUTPUT_FILE_HL = os.path.join(OUTPUT_PATH, "output_hl.txt")
OUTPUT_FILE_LL = os.path.join(OUTPUT_PATH, "output_ll.txt")


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


def llm_scenario_comprehension(query_hl, query_ll) -> bool:
    """
    :brief: This function checks if the LLM has correctly understood the scenario for both the high-level and low-level
            descriptions. It first checks the high-level scenario and then the low-level scenario in accordance with the
            high-level scenario.
    :details: The function uses the LLM to check the comprehension of the scenario. It uses only high-level examples for
                the high-level scenario and both high-level and low-level examples for the low-level scenario. The path 
                to the high-level examples is defined in the global variable CC_EXAMPLES_HL_PATH and the path to the
                low-level examples is defined in the global variable CC_EXAMPLES_LL_PATH. The number of token is set to 
                1000 since it seems to be working fine for both cases, but it may be necessary to tweak it.
                In order to check if the LLM has correctly understood the scenario, the function checks that the LLM
                returned something successfully, and then checks if the response contains the word 'OK' or 'PROBLEM'.
    :param query_hl: The high-level scenario
    :param query_ll: The low-level scenario
    """
    
    file = open(OUTPUT_FILE_CC, "w+")

    # print("test\n\n\n\n\n\n\n\ntest")
    # raise NotImplementedError("This function is not implemented yet")
    INFO("\r[CC] Checking LLM comprehension of scenario for high-level", imp=True)
    llm_scenario = LLM(
        llm_connection_config_file=LLM_CONF_PATH,
        examples_yaml_file = [CC_EXAMPLES_HL_PATH]
    )
    llm_scenario.max_tokens = 1000

    INFO("\r[CC] Checking LLM comprehension of scenario for high-level", imp=True)
    scenario_query_hl = f"Given the following high-level scenario:\n{query_hl}\nIf you think that there is a problem with the description, then write 'PROBLEM' and describe the problem, otherwise write 'OK'"
    succ, response = llm_scenario.query(scenario_query_hl)
    if succ and "OK" in response:
        MSG(f"\rLLM has correctly understood the scenario\n{response}") 
        file.write(f"HL: {response}\n")
    elif succ and "PROBLEM" in response:
        FAIL(f"\rLLM has not correctly understood the scenario or there is a problem in the scenario\n{response}")
        file.write(f"LLM has not correctly understood the scenario or there is a problem in the scenario\n{response}")
        return False, response
    else: 
        FAIL(f"Problem with the LLM\n{response}")
        sys.exit(1)

    # Check comprehension of both the high-level and low-level scenarios

    llm_scenario = LLM(
        llm_connection_config_file=LLM_CONF_PATH,
        examples_yaml_file = [CC_EXAMPLES_CONFIG_PATH]
    )
    llm_scenario.max_tokens = 1000

    INFO("\r[CC] Checking LLM comprehension of scenario for both levels", imp=True)
    scenario_query = f"Given the following high-level description of a scenario:\n{query_hl}\nAnd the following low-level description of the same scenario\n{query_ll}\nIf you think that there is a problem with the description, then write 'PROBLEM' and describe the problem, otherwise write 'OK'"
    succ, response = llm_scenario.query(scenario_query)
    if succ and "OK" in response:
        MSG(f"\rLLM has correctly understood the scenario\n{response}") 
        file.write(f"LLM has correctly understood the scenario\n{response}\n")
    elif succ and "PROBLEM" in response:
        FAIL(f"\rLLM has not correctly understood the scenario or there is a problem in the scenario\n{response}")
        file.write(f"LLM has not correctly understood the scenario or there is a problem in the scenario\n{response}")
        return False, response
    else: 
        FAIL(f"Problem with the LLM\n{response}")
        sys.exit(1)

    file.close()

    return True, ""


########################################################################################################################


def hl_llm_multi_step(query) -> dict:
    """
    :brief This function uses the LLM to extract the high-level knowledge base, the initial and final states.
    :param query: The query that will be used to extract the knowledge base, initial and final states. 
    :return: A tuple containing the knowledge base and the response from the LLM
    """
    file = open(OUTPUT_FILE_HL, "w+")

    # Extract HL knowledge base
    INFO("\r[HL] Extracting HL knowledge base", imp=True)
    llm = LLM(
        llm_connection_config_file=LLM_CONF_PATH,
        examples_yaml_file = [HL_EXAMPLES_CONFIG_PATH]
    )

    kb = {}

    # Generate knowledge base
    INFO("\r[HL] Generating knowledge base")
    kb_query = "\nGiven that the previous messages are examples, you now have to produce code for the task that follows.\n" +\
        query +\
        "\nWrite the static knowledge base. Remember to specify all the correct predicates and identify which are the predicates that are resources and to wrap it into Markdown tags \"```kb\" and NOT with \"```Prolog\"."
    succ, tmp_response = llm.query(kb_query)
    assert succ == True, "Failed to generate static knowledge base"
    print(succ, tmp_response+'\n')
    file.write(f"KB: {tmp_response}\n")
    scan_and_extract(kb, tmp_response)

    # Generate initial and final states
    INFO("\r[HL] Generating initial and final states")
    states_query = "\nGiven that the previous messages are examples, you know have to produce code for the task that follows.\n" + query + \
        "\nGiven the following static knowledge base\n```kb\n{}\n```".format(kb["kb"]) +\
        "\nWrite the initial and final states, minding to include all the correct predicates. Remember to wrap it into Markdown tags \"```init\" and \"goal\" and NOT with \"```prolog\"."
    succ, tmp_response = llm.query(states_query)
    assert succ == True, "Failed to generate initial and final states"
    print(succ, tmp_response+'\n')
    file.write(f"INIT: {tmp_response}\n")
    scan_and_extract(kb, tmp_response)

    # Generate action set
    INFO("\r[HL] Generating actions set")
    final_query = "\nGiven that the previous messages are examples, you know have to produce code for the task that follows.\n" + query + \
        "Given the following static knowledge base\n```kb\n{}\n```".format(kb["kb"]) + \
        "\nKnowing that the initial state is the following\n```init\n{}\n```".format(kb["init"]) + \
        "\nKnowing that the goal state is the following\n```goal\n{}\n```".format(kb["goal"]) + \
        "\nWrite the set of temporal actions divided into _start and _end actions. Remember to wrap it into Markdown tags \"```actions\" and NOT with \"```prolog\"."
    succ, response = llm.query(final_query)
    assert succ == True, "Failed to generate final state"
    print(succ, response)
    print()
    file.write(f"ACTIONS: {response}\n")
    scan_and_extract(kb, response)

    write_to_file(kb, output_file=OUTPUT_HL_KB_FILE)

    file.close()

    return kb


########################################################################################################################


def ll_llm_multi_step(query, kb) -> dict: 
    hl_kb = """
    ```kb
    {}
    ```
    ```init
    {}
    ```
    ```goal
    {}
    ```
    ```actions
    {}
    ```""".format(kb["kb"], kb["init"], kb["goal"], kb["actions"])
    
    # hl_kb = """
    # ```kb
    # {}
    # ```
    # ```init
    # {}
    # ```
    # ```goal
    # {}
    # ```
    # """.format(kb["kb"], kb["init"], kb["goal"])

    file = open(OUTPUT_FILE_LL, "w+")

    LLM_LL_WARNING = "Remember to prepend the low-level predicates with `ll_` and also to not use the high-level predicates inside the low-level actions as they may lead to errors."

    # Extract LL knowledge base
    INFO("\r[LL] Extract LL knowledge base", imp=True)
    llm = LLM(
        llm_connection_config_file=LLM_CONF_PATH,
        examples_yaml_file = [LL_EXAMPLES_CONFIG_PATH]
    )
    
    # Generate static knowledge-base
    INFO("\r[LL] Generating knowledge base")

    kb_query = (
        "\nYou now have to produce code for the task that follows.\n" + query +
        "Given the following high-level knowledge-base:\n{}\n".format(kb['kb']) + 
        "\n**Your task is to update the general knowledge base to include the new low-level predicates and resources.**" +
        f"\n**Wrap the COMPLETE updated knowledge base (including blocks, agents, positions, ll_predicates, etc.) within Markdown tags ```kb ... ```.**" + 
        f"\n**CRITICAL: Use the tag ```kb``` for the entire updated knowledge base.**" + 
        f"\n{LLM_LL_WARNING}\n" +
        "\nNow, generate the updated low-level knowledge base:"
    )
    succ, response = llm.query(kb_query)
    assert succ == True, "Failed to generate LL KB"
    print(succ, response)
    file.write(f"KB: {response}\n")
    scan_and_extract(kb, response)

    # Generate initial and final states
    INFO("\r[LL] Generating initial and final states")
    states_query = (
        "\nYou now have to produce code for the task that follows.\n" + query +
        "\nGiven the complete low-level knowledge-base generated previously:\n```kb\n{}\n```\n".format(kb["kb"]) + 
        "And given the high-level initial and final states as reference:\n```init\n{}\n```\n```goal\n{}\n```\n".format(kb["init"], kb["goal"]) + 
        "\n**Your task is to update ONLY the initial and final states to reflect the low-level details.**" +
        "\n**Wrap the updated initial state within Markdown tags ```init ... ```.**" + 
        "\n**Wrap the updated final state within Markdown tags ```goal ... ```.**" + 
        "\n**CRITICAL: Output ONLY these two blocks (`init` and `goal`).**" + 
        "\n**Do NOT include the knowledge base (`kb`), actions (`ll_actions`), resources, or any other information.**" +
        f"\n**Do NOT use ```python``` or any other tags besides ```init``` and ```goal```.**" + 
        f"\n{LLM_LL_WARNING}\n" + 
        "\nExample format:\n```init\n% Low-level initial state predicates...\ninitial_state([...]).\n```\n\n```goal\n% Low-level goal state predicates...\ngoal_state([...]).\n```\n" + 
        "\nNow, generate the updated low-level initial and final states:" 
    )

    succ, response = llm.query(states_query)
    assert succ == True, "Failed to query LLM for LL init/goal states" 
    print(succ, response)
    file.write(f"{response}\n") 
    scan_and_extract(kb, response)

    # Check '''init''' and '''goal''' tags
    if "init" not in kb or "goal" not in kb:
        FAIL(f"[LL] LLM failed to generate the required 'init' and/or 'goal' tags in its response. Raw response was:\n{response}")
        raise ValueError("LLM did not produce the expected 'init'/'goal' tags.")
    if "python" in kb:
        print("[WARNING][LL] LLM generated an unexpected 'python' tag during init/goal generation. Ignoring it.")

    # Generate actions set
    INFO("\r[LL] Generating actions set")

    # Specific instructions to assure '''ll_actions''' tag
    ll_actions_query = (
        "\nGiven that the previous messages are examples, you now have to produce code for the task that follows.\n" + query +
        "Given the following high-level knowledge-base:\n{}\n".format(hl_kb) +
        "Given the refactored low-level knowledge-base:\n```kb\n{}\n```\n".format(kb["kb"]) +
        "\nGiven the low-level initial state:\n```init\n{}\n```".format(kb["init"]) + 
        "\nAnd the low-level goal state:\n```goal\n{}\n```".format(kb["goal"]) +
        "\n**Your task is to generate ONLY the set of low-level actions.**" +
        "\n**Wrap the low-level actions within Markdown tags ```ll_actions ... ```.**" + 
        "\n**CRITICAL: Use the tag ```ll_actions``` and NOT ```actions``` or any other tag.**" + 
        "\n**Do NOT include initial state, goal state, knowledge base, or any other information.**" +
        "\n**Just provide the list of low-level actions inside the ```ll_actions``` tag.**" + 
        "\nNow, generate the specific low-level actions for the current problem:" 
    )

    succ, response = llm.query(ll_actions_query)
    assert succ == True, "Failed to query LLM for LL actions"

    print(succ, response)
    file.write(f"{response}\n")

    scan_and_extract(kb, response)

    # Tag correctness verification
    if "ll_actions" not in kb:
        error_message = f"[LL] LLM failed to generate the required 'll_actions' tag in its response."
        # Check if it generates '''actions'''
        fallback_content = kb.get("actions")
        if fallback_content:
            error_message += f" Found 'actions' tag instead. Raw response was:\n{response}"
        else:
            error_message += f" No 'll_actions' or 'actions' tag found. Raw response was:\n{response}"
        FAIL(error_message) 
        raise ValueError("LLM did not produce the expected 'll_actions' tag.")

    # Generate mappings
    mappings_query = (
        "Given that the previous messages are examples, you know have to produce code for the task that follows.\n" + query + \
        "Given the following high-level knowledge-base:\n{}\n".format(hl_kb) + \
        "Given the refactored low-level knowledge-base:\n```kb\n{}\n```\n".format(kb["kb"]) + \
        "Given the initial state:\n```init\n{}\n```\n".format(kb["init"]) + \
        "Given the final state:\n```goal\n{}\n```\n".format(kb["goal"]) + \
        "Given the low-level actions set:\n```actions\n{}\n```\n".format(kb["ll_actions"]) + \
        "\n**Provide the mappings from high-level actions to low-level actions.**" +
        "\n**Remember that the mappings are only for the start actions.**" +
        "\n**Wrap the mappings within Markdown tags ```mappings ... ```.**" + 
        "\n**CRITICAL: Use the tag ```mappings``` and NOT ```kb``` or any other tag.**" + 
        f"\n{LLM_LL_WARNING}\n" +
        "\nNow, generate the mappings:"
    )
    succ, response = llm.query(mappings_query)
    assert succ == True, "Failed to generate LL Mappings" 
    print(succ, response)
    file.write(f"{response}\n") 
    scan_and_extract(kb, response)

    if "mappings" not in kb:
        FAIL(f"[LL] LLM failed to generate the required 'mappings' tag. Raw response was:\n{response}")
        raise ValueError("LLM did not produce the expected 'mappings' tag.")

    file.close()
    write_to_file(kb, output_file=OUTPUT_KB_FILE)

    return kb
    

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
 

def main():
    assert os.path.exists(LLM_CONF_PATH), f"LLM configuration file not found at {LLM_CONF_PATH}"
    assert os.path.exists(CC_EXAMPLES_CONFIG_PATH), f"CC examples path not found at {CC_EXAMPLES_CONFIG_PATH}"
    assert os.path.exists(CC_EXAMPLES_HL_PATH), f"CC high-level examples path not found at {CC_EXAMPLES_HL_PATH}"
    assert os.path.exists(CC_EXAMPLES_LL_PATH), f"CC low-level examples path not found at {CC_EXAMPLES_LL_PATH}"
    assert os.path.exists(LL_EXAMPLES_CONFIG_PATH), f"Low-level examples path not found at {LL_EXAMPLES_CONFIG_PATH}"
    assert os.path.exists(HL_EXAMPLES_CONFIG_PATH), f"High-level examples path not found at {HL_EXAMPLES_CONFIG_PATH}"
    print(HL_EXAMPLES_CONFIG_PATH)

    query_hl = """
    Given an initial state in which there are two blocks b1, b2 in position (1,1) and (2,2) 
    respectively, move the block b1 to position (3,3) and place b2 on top of b1 using an 
    agent, which is initially available and it will also be available at the end.
        """

    query_ll = """
    Given an initial state in which there are two blocks b1, b2 in position
    (1,1) and (2,2) respectively, move the block b1 to position (3,3) and place b2 on top of
    b1 using 1 agent, which is initially available and it will also be available at the end.
    The agent is a car-like robot. Its initial position is (0,0) and it does not matter where
    they are at the end. 
        """

    GENERAL_DIR = os.path.join(os.path.dirname(__file__), 'exps', 'multi-steps', 'blocks_world', '2', 'query')
    assert os.path.exists(GENERAL_DIR), f"General directory not found at {GENERAL_DIR}"
    assert os.path.exists(os.path.join(GENERAL_DIR, 'query_hl.txt')), f"High-level query file not found at {os.path.join(GENERAL_DIR, 'query_hl.txt')}"
    assert os.path.exists(os.path.join(GENERAL_DIR, 'query_ll.txt')), f"Low-level query file not found at {os.path.join(GENERAL_DIR, 'query_ll.txt')}"
    
    with open(os.path.join(GENERAL_DIR, 'query_hl_corrected.txt'), 'r') as file:
        query_hl = file.read()
    query_hl+="\nRemember that the tags are in the Markdown form of ```tag and not <tag>"
    with open(os.path.join(GENERAL_DIR, 'query_ll.txt'), 'r') as file:
        query_ll = file.read()
    query_ll+="\nRemember that the tags are in the Markdown form of ```tag and not <tag> and that the low-level actions have the tag ll_actions and not actions"

    compr, resp = llm_scenario_comprehension(query_hl, query_ll)
    # if not compr:
    # FAIL(f"There was a problem with the comprehension of the scenario {resp}")
    
    if WAIT:
        input("Consistency check finished, press enter to continue...")

    # Use HL LLM to extract HL knowledge base
    hl_kb = hl_llm_multi_step(query_hl)
    write_to_file(hl_kb)

    if WAIT:
        input("HL finished, press enter to continue...")
    hl_kb = read_from_file()

    # use LL LLM to extract LL knowledge base
    kb = ll_llm_multi_step(query_ll, hl_kb)
    write_to_file(kb)

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