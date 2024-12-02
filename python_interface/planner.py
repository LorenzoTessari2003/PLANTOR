import os
import sys

try:
    from python_interface.BT.BT import BehaviourTree
    from python_interface.MILP.MILP import MILPSolver
    from python_interface.Prolog import prolog as PrologLib
except:
    from BT.BT import BehaviourTree
    from MILP.MILP import MILPSolver
    from Prolog import prolog as PrologLib

# from STN import SimpTempNet
# from BT.BT import BehaviourTree


def main():
    data_dict = PrologLib.execTest()#kb_path="/home/enrico/Projects/prolog_planner/output/kb.pl")

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

    # bt = BehaviourTree(stn)
    # bt.draw()
    # bt.tick()


if __name__ == "__main__":
    main()
