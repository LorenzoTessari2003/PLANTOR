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

def main():

    try:
        opts, args = getopt.getopt(sys.argv[1:], "x:H:i:h", ["xml=", "html=", "input="])
    except getopt.GetoptError:
        print("main.py -x <xml_file> -H <html_file> -i <input_file>")
        sys.exit(2)
    
    xml_file = ""
    html_file = ""
    input_kb_file = ""

    for opt, arg in opts:
        if opt in ("-x", "--xml"):
            xml_file = arg
        elif opt in ("-H", "--html"):
            html_file = arg
        elif opt in ("-i", "--input"):
            input_kb_file = arg
        elif opt == "-h":
            print("main.py -x <xml_file> -H <html_file> -i <input_file>")
            sys.exit()

    if input_kb_file != "":
        assert os.path.exists(input_kb_file), "KB file not found"

    data_dict = PrologLib.execTest(kb_path=input_kb_file)

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
    print(f"Done extracting BT to files {xml_file} and {html_file}")


if __name__ == "__main__":
    main()
