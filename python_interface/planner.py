import os
import sys, getopt

try:
    from python_interface.BT.BT import BehaviourTree
    from python_interface.MILP.MILP import MILPSolver
    from python_interface.Prolog import prolog as PrologLib
except:
    from BT.BT import BehaviourTree
    from MILP.MILP import MILPSolver
    from Prolog import prolog as PrologLib

import time

def main_exec(xml_file = "", html_file = "", input_kb_file = "") -> float:
    start = time.time()

    data_dict = PrologLib.execTest(plan_len=6, kb_path=input_kb_file)

    milp_solver = MILPSolver(
        data_dict["tt_actions"],
        data_dict["actions"],
        data_dict["adj_matrix"],
        data_dict["resources"],
        data_dict["resources_list"],
        data_dict["ll_actions_list"]
    )

    milp_solver.solve()
    
    # milp_solver.draw_graph_from_matrix(os.path.join("output", "MILP.html"), open_browser=True)

    bt = milp_solver.extract_BT()

    bt.draw(html_file)
    bt.toXML(xml_file)
    print(f"Done extracting BT to files {xml_file} and {html_file} in {time.time() - start} seconds")


def run_tests(plan_len, input_kb_file):
    assert os.path.exists(input_kb_file), f"KB file not found: {input_kb_file}"

    def write_to_csv(data):
        import csv
        with open("results.csv", "a+") as f:
            writer = csv.writer(f)
            writer.writerows(data)

    def read_from_csv():
        import csv
        with open("results.csv", "r") as f:
            reader = csv.reader(f)
            return list(reader)

    start = time.time()

    data_dict = PrologLib.execTest(plan_len=plan_len, kb_path=input_kb_file)

    prolog_planner_time = time.time() - start
    start = time.time()

    milp_solver = MILPSolver(
        data_dict["tt_actions"],
        data_dict["actions"],
        data_dict["adj_matrix"],
        data_dict["resources"],
        data_dict["resources_list"],
        data_dict["ll_actions_list"]
    )

    milp_solver.solve()
    
    # milp_solver.draw_graph_from_matrix(os.path.join("output", "MILP.html"), open_browser=True)

    milp_solver.extract_BT()

    comp_time = time.time() - start
    print(f"Done extracting BT to files {xml_file} and {html_file} in {comp_time} seconds")
    
    write_to_csv([[input_kb_file, prolog_planner_time, comp_time]])



if __name__ == "__main__":
    help_str = "main.py [-t] -x <xml_file> -H <html_file> -i <input_file>\n-t is optional and will run the run_tests function"
    try:
        opts, args = getopt.getopt(sys.argv[1:], "tl:x:H:i:h", ["xml=", "html=", "input="])
    except getopt.GetoptError:
        print(help_str)
        sys.exit(2)
    
    xml_file = ""
    html_file = ""
    input_kb_file = ""

    run_tests_flag = False

    for opt, arg in opts:
        if opt in ("-x", "--xml"):
            xml_file = arg
        elif opt in ("-H", "--html"):
            html_file = arg
        elif opt in ("-i", "--input"):
            input_kb_file = arg
        elif opt == "-t":
            run_tests_flag = True
        elif opt == "-l":
            plan_len = int(arg)
        elif opt == "-h":
            print(help_str)
            sys.exit()

    if input_kb_file != "":
        assert os.path.exists(input_kb_file), "KB file not found"

    if run_tests_flag:
        run_tests(plan_len, input_kb_file)
    else:
        main_exec(xml_file, html_file, input_kb_file)
