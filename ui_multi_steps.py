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
    kb_query = "\nYou know have to produce code for the task that follows.\n" + query + \
        "Given the following high-level knowledge-base:\n{}\n".format(kb['kb']) + \
        "\nUpdate only the generals knowledge base to contain the new low-level predicates and the resources. {}".format(LLM_LL_WARNING) 
    succ, response = llm.query(kb_query)
    assert succ == True, "Failed to generate LL KB"
    print(succ, response)
    file.write(f"KB: {response}\n")
    scan_and_extract(kb, response)

    # Generate initial and final states
    INFO("\r[LL] Generating initial and final states")
    states_query = "\nYou know have to produce code for the task that follows.\n" + query + \
        "Given the low-level knowledge-base:\n```kb\n{}\n```\n".format(kb["kb"]) + \
        "Given the high-level initial and final states:\n```init\n{}\n```\n```goal\n{}\n```\n".format(kb["init"], kb["goal"]) + \
        "\nUpdate the initial and final states. Mind to include all the necessary predicates. {}".format(LLM_LL_WARNING)
        # "Given the low-level actions set:\n```actions\n{}\n```\n".format(kb["ll_actions"]) + \
    succ, response = llm.query(states_query)
    assert succ == True, "Failed to generate LL KB"
    print(succ, response)
    file.write(f"INIT: {response}\n")
    scan_and_extract(kb, response)

    # Generate actions set
    INFO("\r[LL] Generating actions set")
    ll_actions_query = "\nGiven that the previous messages are examples, you know have to produce code for the task that follows.\n" + query + \
        "Given the following high-level knowledge-base:\n{}\n".format(hl_kb) + \
        "Given the refactored low-level knowledge-base:\n```kb\n{}\n```\n".format(kb["kb"]) + \
        "\nWrite the low-level actions set. {}".format(LLM_LL_WARNING)
    succ, response = llm.query(ll_actions_query)
    assert succ == True, "Failed to generate LL KB"
    print(succ, response)
    file.write(f"ACTIONS: {response}\n")
    scan_and_extract(kb, response)

    # Generate mappings
    INFO("\r[LL] Generating mappings")
    mappings_query = "\nGiven that the previous messages are examples, you know have to produce code for the task that follows.\n" + query + \
        "Given the following high-level knowledge-base:\n{}\n".format(hl_kb) + \
        "Given the refactored low-level knowledge-base:\n```kb\n{}\n```\n".format(kb["kb"]) + \
        "Given the initial state:\n```init\n{}\n```\n".format(kb["init"]) + \
        "Given the final state:\n```goal\n{}\n```\n".format(kb["goal"]) + \
        "Given the low-level actions set:\n```actions\n{}\n```\n".format(kb["ll_actions"]) + \
        "\nProvide the mappings from high-level actions to low-level actions. Remember that the mappings are only for the start actions. {}".format(LLM_LL_WARNING)
    succ, response = llm.query(mappings_query)
    assert succ == True, "Failed to generate LL KB"
    print(succ, response)
    file.write(f"MAPPINGS: {response}\n")
    scan_and_extract(kb, response)

    file.close()
    write_to_file(kb, output_file=OUTPUT_KB_FILE)

    return kb 
    

########################################################################################################################


def find_plan(kb_file = OUTPUT_KB_FILE, xml_file = OUTPUT_BT_FILE, stn_file = "", html_file = "") -> planner.BehaviourTree:
    # Find plan
    INFO(f"Finding plan for kb in {kb_file}")

    if not os.path.exists(kb_file):
        raise FileNotFoundError(f"Knowledge base file not found at {kb_file}")
    
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

    query_hl = """
The scenario involves two distinct locations, referred to as Location1 and Location2. These two locations are directly connected, allowing for movement between them.
Containers:

    There are two containers in the system:
        Container c1 is initially located in Location1, placed on the ground.
        Container c2 is also in Location1, positioned on top of c1.

Robot:

    A robot, designated as Robot r1, is initially situated in Location1.
    The robot is capable of transporting a container from one location to another. However, to do so:
        The container must be placed on top of the robot.
        The robot can only move while carrying one container at a time.
        The robot cannot move if the container it is carrying is obstructed by another container.

Cranes:

    Each location is equipped with a crane:
        The crane in Location1 operates only within that location.
        The crane in Location2 operates exclusively within Location2.
    Cranes are versatile and capable of performing the following operations:
        Moving a container from the ground to the top of another container within the same location.
        Loading a container onto the robot or unloading a container from the robot. Container could be everywhere but has to be clear
        Placing a container on the ground in the same location, so cannot place a container in a different location (e.g crane 1 is located
        in location1 so can only operate in location1 not in other).
    A crane can only manipulate a container if the container is clear, meaning there is nothing on top of it.
    When executing an action the crane is busy so it could not execute any other action till the finish of the action.

Goal:

    By the end of the operation:
        Container c2 must be relocated to Location2.
        Container c1 must remain in its original position in Location1.

This setup requires a sequence of coordinated actions involving the robot and the cranes to achieve the desired arrangement of containers.
    """


    query_ll = """
Let the container, crane and robot and their positions be described in the high-level part.
There is available only one robot that can:
- move_start(robot, locationFrom, locationTo), which makes the robot starting to move from position (x1,y1) to position (x2,y2).
- move_end(robot, locationFrom, locationTo), which completes the movement of the robot from position (x1,y1) to position (x2,y2).
There are one crane for location that can:
- go_to_c_start(crane, container), which makes the crane move on the top of the container.
- go_to_c_end(crane, container), which completes the movement of the crane to go on the top of the container.
- close_start(crane), which makes the gripper starting to close.
- close_end(crane), which indicates the gripper has closed.
- open_start(crane), which makes the gripper starting to open.
- open_end(crane), which indicates the gripper has opened.
Remember to use the appropriate tags for the code you produce and not to use prolog tags.
Moreover, remember that the low-level actions must not contain high-level predicates. Low-level actions must only contain predicates that start with 'll_'.
    """

    GENERAL_DIR = os.path.join(os.path.dirname(__file__), 'exps', 'multi-steps', 'arch', '2', 'query')
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
    #     FAIL(f"There was a problem with the comprehension of the scenario {resp}")
        
    
    if WAIT:
        input("Consistency check finished, press enter to continue...")

    # Use HL LLM to extract HL knowledge base
    # hl_kb, response = hl_llm(query_hl)
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
    # kb = read_from_file()

    # Take the whole knowledge base and find plan
    # bt = find_plan(kb)

    # Execute the plan
    # execute_plan(bt)

    INFO("ALL DONE!")



if __name__ == "__main__":
    main()