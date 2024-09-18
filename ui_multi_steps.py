# This file is the main file which coordinates the different aspects of the planner. 

from python_interface import planner
from LLM.LLM import LLM
import re
import os, sys

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

def INFO(*args, **kwargs):
    if 'imp' in kwargs and kwargs['imp']:
        kwargs.pop('imp')
        print(bcolors.OKGREEN, end="")
        print(bcolors.BOLD, end="")
        print(*args, **kwargs)
        print(bcolors.ENDC, end="")
    else:
        print(bcolors.OKGREEN, end="")
        print(*args, **kwargs)
        print(bcolors.ENDC, end="")

def FAIL(*args, **kwargs):
    print(bcolors.FAIL, end="")
    print(*args, **kwargs)
    print(bcolors.ENDC, end="")

def MSG(*args, **kwargs):
    print(bcolors.OKBLUE, end="")
    print(*args, **kwargs)
    print(bcolors.ENDC, end="")


def scan_and_extract(kb, response, hierarchy=False):
    pattern = re.compile(r'\`\`\`\s*(\w+)\s*([^\`]*?)\`\`\`', re.DOTALL)
    matches = pattern.findall(response)
    for Key, value in matches:
        key = Key.lower().replace(" ", "")
        value = value.strip()

        if value == "":
            continue

        kb[key] = value


def llm_scenario_comprehension(query_hl, query_ll) -> bool:
    llm_scenario = LLM(
        llm_connection_config_file=os.path.join(os.path.dirname(__file__), 'LLM', 'conf/gpt4o.yaml'),
        examples_yaml_file = ["./LLM/few-shots-cc.yaml"]
    )

    llm_scenario.max_tokens = 1000

    INFO("\r[CC] Checking LLM comprehension of scenario for high-level", imp=True)
    scenario_query_hl = f"Given the following scenario:\n{query_hl}\nIf you think that there is a problem with the description, then write 'PROBLEM' and describe the problem, otherwise write 'OK'"
    succ, response = llm_scenario.query(scenario_query_hl)
    if succ and "OK" in response:
        MSG("\rLLM has correctly understood the scenario") 
        print(response)
    elif succ and "PROBLEM" in response:
        FAIL(f"\rLLM has not correctly understood the scenario or there is a problem in the scenario\n{response}")
        return
    else: 
        FAIL(f"Problem with the LLM\n{response}")
        sys.exit(1)

    # INFO("\r[CC] Checking LLM comprehension of scenario for low-level", imp=True)
    # scenario_query_ll = f"Given the following scenario:\n{query_ll}\nIf you think that there is a problem with the description, then write 'PROBLEM' and describe the problem, otherwise write 'OK'"
    # succ, response = llm_scenario.query(scenario_query_ll)
    # if succ and "OK" in response:
    #     MSG("\rLLM has correctly understood the scenario") 
    #     print(response)
    # elif succ and "PROBLEM" in response:
    #     FAIL(f"\rLLM has not correctly understood the scenario or there is a problem in the scenario\n{response}")
    #     return
    # else: 
    #     FAIL(f"Problem with the LLM\n{response}")
    #     sys.exit(1)

    # INFO("\r[CC] Checking LLM comprehension of scenario for both scenarios", imp=True)
    # scenario_query = f"Given the following high-level description of a scenario:\n{query_hl}\nAnd the following low-level description of the same scenario\n{query_ll}\nIf you think that there is a problem with the description, then write 'PROBLEM' and describe the problem, otherwise write 'OK'"
    # succ, response = llm_scenario.query(scenario_query)
    # if succ and "OK" in response:
    #     MSG("\rLLM has correctly understood the scenario") 
    #     print(response)
    # elif succ and "PROBLEM" in response:
    #     FAIL(f"\rLLM has not correctly understood the scenario or there is a problem in the scenario\n{response}")
    #     return False
    # else: 
    #     FAIL(f"Problem with the LLM\n{response}")
    #     sys.exit(1)

    return True


def hl_llm_multi_step(query) -> tuple:   
    # Extract HL knowledge base
    INFO("\r[HL] Extracting HL knowledge base", imp=True)
    llm = LLM(
        llm_connection_config_file=os.path.join(os.path.dirname(__file__), 'LLM', 'conf/gpt4o.yaml'),
        examples_yaml_file = ["./LLM/few-shots-hl.yaml"]
    )

    kb = {}

    # Generate knowledge base
    INFO("\r[HL] Generating knowledge base")
    kb_query = "\nGiven that the previous messages are examples, you now have to produce code for the task that follows.\n" +\
        query +\
        "\nWrite the static knowledge base. Remember to specify all the correct predicates and identify which are the predicates that are resources and to wrap it into \"kb\" tags and not prolog tags."
    succ, response = llm.query(kb_query)
    assert succ == True, "Failed to generate final state"
    print(succ, response)
    print()
    scan_and_extract(kb, response)

    # Generate initial and final states
    INFO("\r[HL] Generating initial state")
    states_query = "\nGiven that the previous messages are examples, you know have to produce code for the task that follows.\n" + query + \
        "\nGiven the following static knowledge base\n```kb\n{}\n```".format(kb["kb"]) +\
        "\nWrite the initial and final states, minding to include all the correct predicates. Remember to wrap it into \"init\" and \"goal\" tags and not prolog tags."
    succ, response = llm.query(states_query)
    assert succ == True, "Failed to generate initial state"
    print(succ, response)
    print()
    scan_and_extract(kb, response)

    # # Generate action set
    # INFO("\r[HL] Generating actions set")
    # final_query = "\nGiven that the previous messages are examples, you know have to produce code for the task that follows.\n" + query + \
    #     "Given the following static knowledge base\n```kb\n{}\n```".format(kb["kb"]) + \
    #     "\nKnowing that the initial state is the following\n```init\n{}\n```".format(kb["init"]) + \
    #     "\nWrite action set. Remember to wrap it into \"actions\" tags and not prolog tags."
    # succ, response = llm.query(final_query)
    # assert succ == True, "Failed to generate final state"
    # print(succ, response)
    # print()
    # scan_and_extract(kb, response)

    return kb, response


def ll_llm_multi_step(query, kb) -> tuple: 
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
    # ```actions
    # {}
    # ```""".format(kb["kb"], kb["init"], kb["goal"], kb["actions"])
    
    hl_kb = """
    ```kb
    {}
    ```
    """.format(kb["kb"])

    # Extract LL knowledge base
    INFO("\r[LL] Extract LL knowledge base", imp=True)
    llm = LLM(
        llm_connection_config_file=os.path.join(os.path.dirname(__file__), 'LLM', 'conf/gpt4o.yaml'),
        examples_yaml_file = ["./LLM/few-shots-ll.yaml"]
    )
    
    valid = True

    # Generate static knowledge-base
    INFO("\r[LL] Generating knowledge base")
    kb_query = "\nGiven that the previous messages are examples, you know have to produce code for the task that follows.\n" + query + \
        "Given the following high-level knowledge-base:\n{}\n".format(kb['kb']) + \
        "\nUpdate only the generals knowledge base to contain the new low-level predicates and the resources"
    succ, response = llm.query(kb_query)
    assert succ == True, "Failed to generate LL KB"
    print(succ, response)
    scan_and_extract(kb, response)

    # Generate initial and final states
    INFO("\r[LL] Generating initial and final states")
    states_query = "\nGiven that the previous messages are examples, you know have to produce code for the task that follows.\n" + query + \
        "Given the low-level knowledge-base:\n```kb\n{}\n```\n".format(kb["kb"]) + \
        "Given the high-level initial and final states:\n```init\n{}\n```\n```goal\n{}\n```\n".format(kb["init"], kb["goal"]) + \
        "\nUpdate the initial and final states. Mind to include all the necessary predicates."
        # "Given the low-level actions set:\n```actions\n{}\n```\n".format(kb["ll_actions"]) + \
    succ, response = llm.query(states_query)
    assert succ == True, "Failed to generate LL KB"
    print(succ, response)
    scan_and_extract(kb, response)

    # # Generate actions set
    # print(bcolors.OKGREEN, "\r[LL] Generating actions set", bcolors.ENDC)
    # ll_actions_query = "\nGiven that the previous messages are examples, you know have to produce code for the task that follows.\n" + query + \
    #     "Given the following high-level knowledge-base:\n{}\n".format(hl_kb) + \
    #     "Given the refactored low-level knowledge-base:\n```kb\n{}\n```\n".format(kb["kb"]) + \
    #     "\nWrite the low-level actions set."
    # succ, response = llm.query(ll_actions_query)
    # assert succ == True, "Failed to generate LL KB"
    # # print(succ, response)
    # scan_and_extract(kb, response)

    # # Generate mappings
    # print(bcolors.OKGREEN, "\r[LL] Generating mappings", bcolors.ENDC)
    # mappings_query = "\nGiven that the previous messages are examples, you know have to produce code for the task that follows.\n" + query + \
    #     "Given the following high-level knowledge-base:\n{}\n".format(hl_kb) + \
    #     "Given the refactored low-level knowledge-base:\n```kb\n{}\n```\n".format(kb["kb"]) + \
    #     "Given the low-level actions set:\n```actions\n{}\n```\n".format(kb["ll_actions"]) + \
    #     "Given the initial state:\n```init\n{}\n```\n".format(kb["init"]) + \
    #     "Given the final state:\n```goal\n{}\n```\n".format(kb["goal"]) + \
    #     "\nProvide the mappings from high-level actions to low-level actions. Remember that the mappings are only for the start actions."
    # succ, response = llm.query(mappings_query)
    # assert succ == True, "Failed to generate LL KB"
    # # print(succ, response)
    # scan_and_extract(kb, response)

    return kb, response
    

def find_plan(knowledge_base) -> planner.BehaviourTree:
    # Find plan
    print("Find plan")
    

def execute_plan(plan):
    # Execute plan
    print("Execute plan")


def main():
    # query = input("Describe your problem: ")

    query_hl = """
    There are 5 blocks on a table. Block b1 is in position (2,2), block b2 is in position (4,4), block b4 is in position
    (8,8). Block b5 is on top of block b1 and block b3 is on top of block b2. There are 2 agents with a robotic arm. 
    They can:
     - move a block from on top of a block to the table;
     - move a block from the table to another position on the table;
     - move a block from on top of a block and place it on top of another block;
     - move a block from the table and place it on top of another block.
    The goal is to put block b1 on top of block b5 in position (5,5). 
    """

    query_hl = """
    There are 7 blocks on a table. In the initial state of the simulation, block b1 is in position (2,2), block b2 is in 
    position (4,4), block b4 is in position (8,8). Block b5 is on top of block b1 in position (2,2) and block b3 is on 
    top of block b2 in position (4,4). After moving the blocks around during the simulation, at the end, we have b5 on 
    top of block b4, both in position (8,8), and the remaining blocks b1, b2 and b3 are in the same positions as in the 
    initial state. Other blocks, such as b6, are assigned random positions on the table. Please explicitly assign
    these positions.
    There are 8 agents with a robotic arm. 
    """

    # query_hl = """
    # There are 3 blocks on a table. In the initial state of the simulation, block b1 is in position (1,1), block b2 is in 
    # position (2,2), block b3 is in position (3,3). After the execution of the plan, b1 on the table in position (1,1), 
    # b2 is on the table in position (2,2) and b3 is on top of b1 in position (1,1).
    # There are two available agents that can carry out the task. They are available at the beginning and will be 
    # available at the end. 
    # """


    query_ll = """
    There are 3 blocks on a table. In the initial state of the simulation, block b1 is in position (1,1), block b2 is in 
    position (2,2), block b3 is in position (3,3). After the execution of the plan, b1 on the table in position (1,1), 
    b2 is on the table in position (2,2) and b3 is on top of b1 in position (1,1).
    There are two available agents that can carry out the task. They are available at the beginning and will be 
    available at the end. The agents are actually robotic arms that can pick up blocks and move them around. At the 
    beginning, the arms are in position (0,0), (10,10), while we do not care were they are at the end.
    """
    # Remember to prepend the low-level predicates with 'll_'.

    # query_ll = """
    # There are 5 blocks on a table. Block b1 is in position (2,2), block b2 is in position (4,4), block b4 is in position
    # (8,8). Block b5 is on top of block b1 and block b3 is on top of block b2. There are 2 agents with a robotic arm. The
    # agents can move the arm from a position (x1,y1) to another position (x2,y2). They can also pick up a block from the 
    # table or pick up a block from on top of another block. They can also put a block on the table or put a block on top
    # of another block. Finally, they can use the gripper to grasp a block or release a block. The goal is to put block b1
    # on top of block b5 in position (5,5).
    # The API that we have available are:
    # move_arm(arm, x1, y1, x2, y2), which moves a robotic arm from position (x1,y1) to position (x2,y2).
    # close(arm), which makes the gripper close.
    # open(arm), which makes the gripper open.
    # """
    # Remember to prepend the low-level predicates with 'll_'.

    # query_ll = "Nothing to do here"

    wait = True
    # wait = False

    if not llm_scenario_comprehension(query_hl, query_ll):
        FAIL("There was a problem with the comprehension of the scenario")
        return
    
    if wait:
        input("Press enter to continue...")

    # Use HL LLM to extract HL knowledge base
    # hl_kb, response = hl_llm(query_hl)
    hl_kb, response = hl_llm_multi_step(query_hl)

    if wait:
        input("Press enter to continue...")

    # use LL LLM to extract LL knowledge base
    # kb, _ = ll_llm_multi_step(query_ll.format(response), hl_kb)

    # for key, value in kb.items():
    #     print(key, value)

    with open("kb.pl", "w") as file:
        file.write("% This file was automatically generated by the LLM system\n")
        first_line = True
        for key, value in hl_kb.items():
            if not first_line:
                file.write("\n")
            file.write(f"%%%%%%%%%%%%%%%%%%%%%%%\n% {key}\n%%%%%%%%%%%%%%%%%%%%%%%\n{value}\n")
            first_line = False

        file.write("""
:- ensure_loaded('prolog_planner/examples/blocks_world/actions.pl').
:- ensure_loaded('prolog_planner/examples/blocks_world/mappings.pl').
""")
                   

    # Take the whole knowledge base and find plan
    # bt = find_plan(kb)

    # Execute the plan
    # execute_plan(bt)


if __name__ == "__main__":
    main()