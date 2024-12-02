import collections
from ortools.sat.python import cp_model

try:
    from python_interface.utility.Graph.Graph import Graph
    from python_interface.BT.BT import BehaviourTree
except:
    from utility.Graph.Graph import Graph
    from BT.BT import BehaviourTree

hl_t = 0
ll_t = 0

class MILPSolver(cp_model.CpSolver): 
    def __init__(self, tta_actions, actions, adj_matrix, resources, resources_list, ll_actions_list):
        super().__init__()
        self.status = cp_model.UNKNOWN

        self.tta_actions = tta_actions
        self.actions = actions
        self.adj_matrix = adj_matrix
        self.resources = resources
        self.resources_list = resources_list
        self.ll_actions_list = ll_actions_list

        print("Creating MILP model")
        print(f"tta_actions: [{type(self.tta_actions)}]", self.tta_actions)
        print(f"actions: [{type(self.actions)}]", self.actions)
        print(f"resources: [{type(self.resources)}]", self.resources)
        print(f"resources_list: [{type(self.resources)}]", self.resources_list)
        print(f"ll_actions_list: [{type(self.ll_actions_list)}]", self.ll_actions_list)

        from math import log

        for space_id in range(int(log(len(self.adj_matrix), 10)) + 2):
            print('', end=" ")
        for col_id in range(len(self.adj_matrix[0])):
            print(f"{col_id:>{len(str(len(self.adj_matrix)))}}", end=" ")
        print()
        
        for row_id in range(len(self.adj_matrix)):
            self.adj_matrix[row_id][row_id] = 0
            #print row_id occupying as many spaces as math.log(len(self.adj_matrix), 10) + 1
            print(f"{row_id:>{len(str(len(self.adj_matrix)))}}", end=" ")
            for col_id in range(len(self.adj_matrix[row_id])):
                print(f"{self.adj_matrix[row_id][col_id]:>{len(str(len(self.adj_matrix)))}}", end=" ")
            print()
        print(self.resources)

        # Create dependency graph
        edges = []
        for i in range(len(self.adj_matrix)):
            for j in range(len(self.adj_matrix[i])):
                if self.adj_matrix[i][j] == 1:
                    edges.append(
                        (
                            (self.actions[i]),
                            (self.actions[j]),
                        )
                    )

        self.graph = Graph(edges)
        
        self.__verify__()

        # Create model
        self.model = cp_model.CpModel()

        task_type = collections.namedtuple("tta_type", "start end interval")
        
        horizon = int(1e12)
        self.all_tta = {}
        self.all_resources = collections.defaultdict(list)

        print('Printing tta_actions:')
        for tta in self.tta_actions.keys():
            prefix = f"{tta}_"
            start_var = self.model.NewIntVar(0, horizon, prefix + "s")
            end_var = self.model.NewIntVar(0, horizon, prefix + "e")
            duration = self.model.NewIntVar(self.tta_actions[tta]['l'], self.tta_actions[tta]['u'], prefix + "d")
            interval_var = self.model.NewIntervalVar(start_var, duration, end_var, prefix + "i")
            self.all_tta[tta] = task_type(start=start_var, end=end_var, interval=interval_var)
        
            print(self.tta_actions[tta])
            if self.tta_actions[tta]['R'] != []:
                for res in self.tta_actions[tta]['R']:
                    self.all_resources[res].append(self.all_tta[tta].interval)

        self.__set_constraints__()

        # Objective function 
        self.model.minimize(self.all_tta['0'].end)


    def __verify__(self):
        """
        Verify the consistency of the adjacency matrix and actions list.

        Raises:
            Exception: - If the length of the adjacency matrix does not match the length of the actions list,
                       - If the length of the adjacency matrix does not match the length of the ttas list multiplied by 2.
                       - If a snap actions was not found in the ttas.
                       - If a snap action is found in more than one tta.
                       - All nodes must have a path to the final node.
        """        
        assert len(self.adj_matrix) == len(self.actions) and len(self.adj_matrix[0]) == len(self.actions), \
            "Error: Adjacency matrix and actions list length mismatch"
        
        assert len(self.actions) == 2*len(self.tta_actions), "Error: Actions and TTAs length mismatch"

        for snap_action in self.actions:
            snap_id = snap_action.split("_")[0]
            counter = 0
            for tta_action, value in self.tta_actions.items():
                if  value['s'].split('_')[0] == snap_id or \
                    value['e'].split('_')[0] == snap_id:
                    counter += 1
            if counter < 1:
                raise Exception(f"Error: Snap action {snap_action} not found in TTAs, {self.tta_actions}")
            elif counter > 1:
                raise Exception(f"Error: Snap action {snap_action} found in more than one TTA, {self.tta_actions}")

        for node in self.graph.nodes:
            (cycles, edges) = self.graph.find_cycle(node)
            if cycles:
                print(edges)
                raise Exception("Error: Graph has at least one cycle between nodes")
        
        import networkx as nx
        for node in self.graph.nodes:
            if node == self.tta_actions['0']['e']:
                continue
            assert nx.has_path(self.graph, node, self.tta_actions['0']['e']), f"Error: No path from {node} to final node"


    def draw_graph_from_matrix(self, title, rm_redundant=False, open_browser=False):
        self.graph.draw(title=title, rm_redundant=rm_redundant, open_browser=open_browser)


    def __tta_from_snap_action(self, action : str) -> str:
        """
        Get the TTA from a snap action.

        Since at the beginning we verify the consistency of the data, this method should always return a valid TTA.

        Args:
            action (str): The snap action.

        Returns:
            str: The TTA.
        """
        snap_id = action.split("_")[0]
        for tta_action, value in self.tta_actions.items():
            if  value['s'].split('_')[0] == snap_id or \
                value['e'].split('_')[0] == snap_id:
                return tta_action
        
        return ""


     ######   #######  ##    ##  ######  ######## ########     ###    #### ##    ## ########  ######  
    ##    ## ##     ## ###   ## ##    ##    ##    ##     ##   ## ##    ##  ###   ##    ##    ##    ## 
    ##       ##     ## ####  ## ##          ##    ##     ##  ##   ##   ##  ####  ##    ##    ##       
    ##       ##     ## ## ## ##  ######     ##    ########  ##     ##  ##  ## ## ##    ##     ######  
    ##       ##     ## ##  ####       ##    ##    ##   ##   #########  ##  ##  ####    ##          ## 
    ##    ## ##     ## ##   ### ##    ##    ##    ##    ##  ##     ##  ##  ##   ###    ##    ##    ## 
     ######   #######  ##    ##  ######     ##    ##     ## ##     ## #### ##    ##    ##     ######  
    def __set_constraints__(self):
        # The first task must start at 0
        self.model.Add(self.all_tta['0'].start == 0)
        
        # Snap actions can start after all the previous snap actions have started
        for idr in range(len(self.adj_matrix)):
            for idc in range(len(self.adj_matrix[idr])):
                if self.adj_matrix[idr][idc] == 1:
                    tta_id1 = self.__tta_from_snap_action(self.actions[idr].split("_")[0])
                    tta_id2 = self.__tta_from_snap_action(self.actions[idc].split("_")[0])
                    # print(f"Adding constraint that {self.actions[idc]} starts after {self.actions[idr]}")
        
                    if "_s" in self.actions[idc]:
                        if "_s" in self.actions[idr]:
                            self.model.Add(self.all_tta[tta_id2].start >= self.all_tta[tta_id1].start)
                            # print(f"Adding constraint that {tta_id2}_s starts after {tta_id1}_s")
                        elif "_e" in self.actions[idr]:
                            self.model.Add(self.all_tta[tta_id2].start >= self.all_tta[tta_id1].end)
                            # print(f"Adding constraint that {tta_id2}_s starts after {tta_id1}_e")
                        else:
                            raise Exception(f"Invalid action {idr} {self.actions[idr]}, no _s or _e in {self.actions[idr]}")
                    
                    elif "_e" in self.actions[idc]:
                        if "_s" in self.actions[idr]:
                            self.model.Add(self.all_tta[tta_id2].end >= self.all_tta[tta_id1].start)
                            # print(f"Adding constraint that {tta_id2}_e starts after {tta_id1}_s")
                        elif "_e" in self.actions[idr]:
                            self.model.Add(self.all_tta[tta_id2].end >= self.all_tta[tta_id1].end)
                            # print(f"Adding constraint that {tta_id2}_e starts after {tta_id1}_e")
                        else:
                            raise Exception(f"Invalid action {idr} {self.actions[idr]}")
                    
                    # else:
                    #     raise Exception(f"Invalid action {idc} {self.actions[idc]}, no _s or _e in {self.actions[idc]}")
                    
        # The last task must end at the end
        for tta in self.all_tta.keys():
            if tta != '0':
                self.model.add(self.all_tta[tta].end <= self.all_tta['0'].end)
        
        # The number of tasks using a resource must be less than the resource capacity
        for res in self.all_resources.keys():
            for n_tta in self.all_resources[res]:
                print(n_tta, end=" ")
            print("\n", str([1 for n_tta in self.all_resources[res]]))
            self.model.AddCumulative(self.all_resources[res], [1 for n_tta in self.all_resources[res]], self.resources[res])

    def solve(self) -> None:
        """
        Solve the MILP problem.

        This method solves the MILP problem using the underlying solver. It sets the status of the solution and prints the solution if it is found. It also prints statistics about the solving process.

        Returns: 
            None
        """
        self.status = super().solve(self.model)
        
        if self.status == cp_model.OPTIMAL or self.status == cp_model.FEASIBLE:
            print("Solution found. Assigning values.")
            self.print_solution()
        else:
            print("No solution found.", self.solution_info())

        # Statistics.
        print("\nStatistics")
        print(f"  - conflicts: {self.num_conflicts}")
        print(f"  - branches : {self.num_branches}")
        print(f"  - wall time: {self.wall_time}s")

        self.add_more_enablers()
        

    def print_solution(self) -> None:
        """
        Prints the solution of the MILP problem.

        If the MILP problem has an optimal or feasible solution, this method prints the solution
        by iterating over all tasks and printing their start time, end time, and duration.

        Returns:
            None
        """
        if self.status == cp_model.OPTIMAL or self.status == cp_model.FEASIBLE:
            print("Solution:")

            # Print the start, end, and duration of each task. Format "tta : [start -> end, duration]"
            output = ""
            for tta in self.all_tta.keys():
                start = self.Value(self.all_tta[tta].start)
                end = self.Value(self.all_tta[tta].end)
                duration = self.Value(self.all_tta[tta].interval.size_expr())
                output += f"{tta} : [{start} -> {end}, {duration}]\n"

            print(f"Optimal Schedule Length: {self.objective_value}")
            print(output)

            # Print the start, end, and duration of each task. Format "tta.start(start_t) -> tta.end(end_t), duration]"
            output = ""
            for tta in self.all_tta.keys():
                start = self.Value(self.all_tta[tta].start)
                end = self.Value(self.all_tta[tta].end)
                duration = self.Value(self.all_tta[tta].interval.size_expr())
                output += f"{self.tta_actions[tta]['s']}({start}) -> {self.tta_actions[tta]['e']}({end}), {duration}]\n"
            print(output)

            # self.draw_intervals()


    def is_ll_action(self, action):
        for ll_action in self.ll_actions_list:
            if ll_action in action:
                return True
        return False



    def add_more_enablers(self):
        import os
        for tta1 in self.all_tta.keys():
            for tta2 in self.all_tta.keys():
                if not (self.is_ll_action(self.tta_actions[tta1]['s']) or self.is_ll_action(self.tta_actions[tta2]['s'])):
                    end_tta1 = self.Value(self.all_tta[tta1].end)
                    start_tta2 = self.Value(self.all_tta[tta2].start)

                    if start_tta2 >= end_tta1:
                        self.graph.add_edge(self.tta_actions[tta1]['e'], self.tta_actions[tta2]['s'])

        self.draw_graph_from_matrix(os.path.join("output", "MILP.html"), open_browser=True)



    def extract_BT(self) -> BehaviourTree:
        global debug
        # root = BTNode()       

        class Node():
            def __init__(self, name):
                self.name = name
                self.children = []
                self.start_time, self.end_time = 0, 0
                self.resources = []
                self.type = "ActionNode"

            def add_child(self, child):
                if isinstance(child, Node) and child not in self.children:
                    self.children.append(child)

            def __str__(self):
                return f"{self.name} [{self.start_time}, {self.end_time}] -> {[x.name for x in self.children]}"

        # First create a dictionary where the key is the name of the action and the value is a tuple with start and end times
        times_for_ttas = {}
        for tta in self.all_tta.keys():
            times_for_ttas[self.tta_actions[tta]['s']] = (self.Value(self.all_tta[tta].start), self.Value(self.all_tta[tta].end))
        times_for_sas = {}
        for tta in self.all_tta.keys():
            times_for_sas[self.tta_actions[tta]['s']] = self.Value(self.all_tta[tta].start)
            times_for_sas[self.tta_actions[tta]['e']] = self.Value(self.all_tta[tta].end)

        print("\n\n", times_for_ttas)
        print("\n\n", times_for_sas)

        root_stn = Node(self.tta_actions['0']['s'])
        root_stn.start_time = self.Value(self.all_tta['0'].start)
        root_stn.end_time = self.Value(self.all_tta['0'].end)


        import networkx as nx
        tree_nodes = {}

        debug = False

        def add_children(node : Node, action_name, graph : nx.DiGraph, visited = [], tab = '', hl_t = 0, ll_t = 0):
            # global hl_t, ll_t
            global debug
            visited.append(action_name)
            out_nodes = [x for (_, x) in graph.out_edges(action_name)]
            # out_nodes.sort()
            if "18_release_start" in action_name or "19_release_end" in action_name or\
                "1_move_table_to_table_start" in action_name :
                debug = True
            else: 
                debug = False
            print(f"{tab}Node {action_name}, has children {out_nodes}") if debug else None

            for child_action in out_nodes:
                if "end_e()" in child_action:
                    continue

                is_child_action_ll_t = self.is_ll_action(child_action)
                is_child_action_start = True
                if "_end(" in child_action:
                    is_child_action_start = False
                elif "_start(" not in child_action:
                    raise Exception(f"Invalid action {child_action}")
                
                print(f"{tab}Testing action {child_action} ({hl_t}, {ll_t})") #if debug else None
                if child_action not in visited:
                    print(f"{tab}Node {child_action} not visited") if debug else None

                    if is_child_action_ll_t:
                        if is_child_action_start:
                            print(f"{tab}Node {child_action} is a start action") if debug else None
                            if times_for_ttas[child_action][0] <= ll_t:
                                print(f"{tab}Node {child_action} starts before {ll_t} >= {times_for_ttas[child_action][0]}") if debug else None
                                child_node = Node(child_action)
                                child_node.start_time = times_for_ttas[child_action][0]
                                tree_nodes[child_action] = child_node
                                node.add_child(child_node)
                                ll_t = times_for_ttas[child_action][0]
                                print(f"{tab}Node {child_action} is a LL action, new time {ll_t}") if debug else None
                                add_children(child_node, child_action, graph, visited, tab+'  ', hl_t, ll_t)
                            else:
                                print(f"{tab}Node {child_action} starts after {ll_t} -> {times_for_ttas[child_action][0]}") if debug else None

                        else:
                            # find corresponding start action
                            start_action = ""
                            for _, value in self.tta_actions.items():
                                if value['e'] == child_action:
                                    start_action = value['s']
                                    break

                            if start_action in tree_nodes:
                                print(f"{tab}Node {child_action} is the end action of {start_action}") if debug else None
                                tree_nodes[start_action].end_time = times_for_sas[child_action]
                                ll_t = times_for_ttas[start_action][1] 
                                
                                child_node = Node(child_action)
                                child_node.end_time = times_for_ttas[start_action][1]
                                node.add_child(child_node)
                                
                                print(f"{tab}Node {child_action} is a LL action, new time {ll_t}") if debug else None
                                add_children(child_node, child_action, graph, visited, tab+'  ', hl_t, ll_t)

                    else:
                        if is_child_action_start:
                            print(f"{tab}Node {child_action} is a start action") if debug else None
                            if times_for_ttas[child_action][0] <= hl_t:
                                print(f"{tab}Node {child_action} starts before {hl_t} >= {times_for_ttas[child_action][0]}") if debug else None
                                child_node = Node(child_action)
                                child_node.start_time = times_for_ttas[child_action][0]
                                tree_nodes[child_action] = child_node
                                node.add_child(child_node)
                                hl_t = times_for_ttas[child_action][0]
                                print(f"{tab}Node {child_action} is a LL action, new time {hl_t}") if debug else None
                                add_children(child_node, child_action, graph, visited, tab+'  ', hl_t, ll_t)
                            else:
                                print(f"{tab}Node {child_action} starts after {hl_t} -> {times_for_ttas[child_action][0]}") if debug else None

                        else:
                            # find corresponding start action
                            start_action = ""
                            for _, value in self.tta_actions.items():
                                if value['e'] == child_action:
                                    start_action = value['s']
                                    break

                            if start_action in tree_nodes:
                                print(f"{tab}Node {child_action} is the end action of {start_action}") if debug else None
                                tree_nodes[start_action].end_time = times_for_sas[child_action]
                                hl_t = times_for_ttas[start_action][1] 
                                
                                child_node = Node(child_action)
                                child_node.end_time = times_for_ttas[start_action][1]
                                node.add_child(child_node)

                                print(f"{tab}Node {child_action} is a LL action, new time {hl_t}") if debug else None
                                add_children(child_node, child_action, graph, visited, tab+'  ', hl_t, ll_t)


                else:
                    print(f"{tab}Node {child_action} already visited") if debug else None
                    continue


        def prune_bt(node, parent = None):
            """
            :brief: Function that, starting from the root of the tree, prunes the tree by removing nodes that are not 
            low-level actions (checked with is_ll_action()) and attaching the children of the removed node to the 
            parent. 
            """
            print(f"Pruning node {node}")
            if parent is None or self.is_ll_action(node.name) or "0_init" in node.name:
                print(f"Node {node.name} is a low-level action")
                for child in node.children:
                    prune_bt(child, node)
            else:
                print(f"Node {node.name} is not a low-level action")
                for child in node.children:
                    parent.add_child(child)
                print(f"parent: {parent}")
                if node in parent.children:
                    parent.children.remove(node)
                    prune_bt(parent, node)
            return

                    
        print('\n\n%%%%%%%%%%%%%%%%%%%%%%%%')
        add_children(root_stn, root_stn.name, self.graph)
        print("\n")
        prune_bt(root_stn)
        print('\n\n%%%%%%%%%%%%%%%%%%%%%%%%')

        

        # resources_x_action = self.extract_resources_x_action()
        # print("resources_x_action:", resources_x_action)
        # self.__assign_values(root, resources_x_action)

        def get_edges(node, edges = []):
            for child in node.children:
                edges.append((node.name, child.name))
                get_edges(child, edges)

        edges = []
        get_edges(root, edges)
        new_graph = Graph(edges)
        print("Displaying graph")
        new_graph.draw(title="BehaviourTree.html", open_browser=True)


    def extract_resources_x_action(self):
        """
        Extracts the resources used by each action.

        This method extracts the resources used by each action and returns a dictionary where the key is the action and the value is a list of resources used by the action.

        Returns:
            dict: The dictionary where the key is the action and the value is a list of resources used by the action.
        """
        resources_x_action = {}
        for tta in self.tta_actions.keys():
            for res in self.tta_actions[tta]['R']:
                if self.tta_actions[tta]['s'] not in resources_x_action:
                    resources_x_action[self.tta_actions[tta]['s']] = []
                resources_x_action[self.tta_actions[tta]['s']].append(res)
            for res in self.tta_actions[tta]['R']:
                if self.tta_actions[tta]['e'] not in resources_x_action:
                    resources_x_action[self.tta_actions[tta]['e']] = []
                resources_x_action[self.tta_actions[tta]['e']].append(res)
        return resources_x_action

    
    def __assign_values(self, bt, resources_x_action, to_visit = [], parent = None) -> None:
        """
        Assigns the values of the variables to the corresponding tasks.

        This method assigns the values of the variables to the corresponding tasks. It iterates over all tasks and assigns their start time, end time, and duration.

        Returns:
            None
        """        
        # If the current node is not the root, assign the resources
        if not "init" in bt.name:
            # If the current node is a leaf node and the to_visit list is empty, return
            if to_visit == [] and bt.children == []:
                print(f"Leaf node {bt.name}")
                return
            
            # Reassign the resources of the father if it has ended
            if parent is not None and parent.end_time <= bt.start_time:
                for (res_name, res) in parent.resources:
                    self.resources_list[res_name].append(res)

            # Assign resources used by the task
            for res_name in resources_x_action[bt.name]:
                # Before assigning the resource, check if it is available. If it is not, MILP fucked up 
                assert len(self.resources_list[res_name]) > 0, f"Error: Resource {res_name} not available"
                bt.resources.append((res_name, self.resources_list[res_name][-1]))
                self.resources_list[res_name] = self.resources_list[res_name][:-1]

            print(f"{bt.name} uses resources {bt.resources}")
        
        # Add the children to the to_visit list and sort them for starting time
        for child in bt.children:
            to_visit.append((child, child.start_time))
        to_visit.sort(key=lambda x: x[1])

        self.__assign_values(to_visit[0][0], resources_x_action, to_visit[1:], bt)

            

        


    def draw_intervals(self):
        """
        Given the solution of the MILP problem, this method draws the intervals of the tasks using matplotlib.
        """

        import matplotlib.pyplot as plt

        for tta in self.all_tta.keys():
            start = self.Value(self.all_tta[tta].start)
            end = self.Value(self.all_tta[tta].end)
            duration = self.Value(self.all_tta[tta].interval.size_expr())

            plt.plot([start, end], [float(tta), float(tta)], 'k-')
            plt.plot([start, start], [float(tta)-0.1, float(tta)+0.1], 'k-')
            plt.plot([end, end], [float(tta)-0.1, float(tta)+0.1], 'k-')

            plt.text(start, float(tta)+0.2, self.tta_actions[tta]['s'], ha='center')
            plt.text(end, float(tta)+0.2, self.tta_actions[tta]['e'], ha='center')

        plt.show()