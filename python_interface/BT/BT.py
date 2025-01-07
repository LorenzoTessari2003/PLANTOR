import networkx as nx
import matplotlib.pyplot as plt
from dict2xml import dict2xml
import pathlib
import re
import json

try:
    from python_interface.STN.STN import SimpTempNet
    from python_interface.BT.BTNode import *
except:
    from STN.STN import SimpTempNet
    from .BTNode import *


RE_ACTION = r".*\d+\_(?P<action_name>[a-zA-Z\_]+)\_start\((?P<args>[^\)]*)\).*"


XML_STANDARD = """
<root BTCPP_format="4" >
    <BehaviorTree ID="MainTree">
        {}
    </BehaviorTree>
</root>
"""


class BehaviourTree(nx.DiGraph):
    def __init__(self, stn : SimpTempNet, root_name):
        super(BehaviourTree, self).__init__()
        assert root_name in stn.nodes, "Root {} not in STN".format(root_name)
        self.root_name_ = ""
        toBTfromSTN(self, stn, root_name)

    def add_bt_node(self, node : BT_NODE):
        if node:
            assert node not in self.nodes, "Node {} already in BT".format(node)
            self.add_node(node)
            self.nodes[node]["type"] = node.type
            if self.root_name_ == "":
                self.root_name_ = node
            if node.get_parent() is not None:
                self.add_edge(node.get_parent(), node)
                node.get_parent().add_child(node)

    def tick(self):
        list(self.nodes.keys())[0].tick()

    def __str__(self):
        pass

    def draw(self):
        self.drawBokeh()

    def drawBokeh(self) -> None:
        # return
        from bokeh.plotting import figure, show, output_file
        from bokeh.models import ColumnDataSource, HTMLLabelSet

        min_node_size = 70000  # Minimum node size
        max_node_size = 10000  # Maximum node size

        # Create a Bokeh plot
        plot = figure(
            width=1600, height=1200
        )

        # Create a layout using from_networkx
        layout = nx.nx_agraph.graphviz_layout(self, prog="dot")  # You can use other layout algorithms as well
        # layout = nx.spring_layout(self)

        # Extract edge endpoints
        edge_start = [edge[0] for edge in self.edges]
        edge_end = [edge[1] for edge in self.edges]

        # Extract node attributes
        Xs = []
        Ys = []
        node_labels = []
        label_lengths = []
        node_sizes = []

        for node in range(len(self.nodes)):
            pos = list(layout.values())[node]
            Xs.append(pos[0])
            Ys.append(pos[1])
            node_labels.append(str(list(self.nodes.keys())[node]))
            label_lengths.append(len(node_labels[-1]))

        for labelLen in label_lengths:
            node_sizes.append((labelLen / max_node_size) * min_node_size)

        # Create a data source for nodes
        node_source = ColumnDataSource(data=dict(
            x=Xs,
            y=Ys,
            labels=node_labels,
            sizes=node_sizes,
        ))

        # Create a data source for edges
        edge_source = ColumnDataSource(data=dict(
            xs=[[layout[start][0], layout[end][0]] for start, end in zip(edge_start, edge_end)],
            ys=[[layout[start][1], layout[end][1]] for start, end in zip(edge_start, edge_end)]
        ))

        # Customize node rendering with dynamically calculated sizes
        nodes = plot.ellipse('x', 'y', width='sizes', height=7, source=node_source, color='white')

        # Display 'label' attribute inside the nodes using LabelSet
        labels = HTMLLabelSet(x='x', y='y', text='labels', source=node_source, level='glyph',
                              text_align='center', text_baseline='middle', text_color='black')

        # Customize edge rendering
        edges = plot.multi_line('xs', 'ys', source=edge_source, line_color='#AAAAAA', line_width=1)

        # Add tooltips to display 'title' attributes for nodes
        # hover = HoverTool()
        # hover.tooltips = [("Label", "@labels")]

        # Remove tooltips for edges
        edges.hover_glyph = None
        edges.nonselection_glyph = None

        # plot.add_tools(hover)

        # Hide Bokeh axes and grid
        plot.axis.visible = False
        plot.grid.visible = False

        # Add nodes, labels, and edge to the same renderers list to ensure proper layering
        plot.renderers.extend([nodes, labels, edges])

        # Show the plot
        output_file(filename="BT.html")
        show(plot)


    def drawMatplotlib(self,):
        plt.close()
        plt.figure(1, figsize=(25, 20), dpi=400)

        pos = nx.nx_agraph.graphviz_layout(self, prog="dot")
        # pos = nx.circular_layout(self)
        # pos = nx.spring_layout(self)
        nx.draw_networkx_nodes(self, pos, node_color='#FFFFFF')
        nx.draw_networkx_edges(self, pos, edge_color='#AAAAAA')
        nx.draw_networkx_labels(self, pos, labels={n: self.nodes[n]['label'] for n in self.nodes})

        plt.savefig("BT.png")


    def toXML(self, filepath = "") -> str:
        if not self.root_name_:
            return ""

        # Read the JSON file
        ros2_dict = {}
        with open(os.path.join("data", "actions.json"), "r") as f:
            ros2_dict = json.load(f)
        
        func_d = {}
        actions_dict = {}

        def toXMLRec(bt, node, dictionary) -> None:
            def get_key_name(action_name):
                return action_name.replace(" ", "_").replace("(", "_").replace(")", "_").replace("[", "_").replace("]", "_").replace(",", "_")

            print(f"toXMLRec: {node} {dictionary}")
            if bt.nodes[node]["type"] == "INIT" or bt.nodes[node]["type"] == "SEQ":
                dictionary["Sequence"] = []
                for _, child in bt.out_edges(node):
                    tmp_dict = {}
                    toXMLRec(bt, child, tmp_dict)
                    if tmp_dict:
                        dictionary["Sequence"].append(tmp_dict)
            
            elif bt.nodes[node]["type"] == "PAR":
                dictionary["Parallel"] = []
                for _, child in bt.out_edges(node):
                    tmp_dict = {}
                    toXMLRec(bt, child, tmp_dict)
                    if tmp_dict:
                        dictionary["Parallel"].append(tmp_dict)
            
            elif bt.nodes[node]["type"] == "ES":
                match = re.match(RE_ACTION, str(node))
                print(node, type(node), node.get_STN_node()["label"])
                if match is not None:
                    # Create action name and check if it is already in the actions dictionary
                    action_name = "Action_"+str(node)
                    if action_name in ros2_dict:
                        raise Exception(f"Action {action_name} already in the dictionary")
                                       
                    # Add entry for action in the XML dictionary
                    dictionary[action_name] = {}

                    # Get the corresponding ROS2 node and service name
                    ros2_node_dict = ros2_dict[match.group("action_name")]
                    ros2_node_name = ros2_node_dict["ros2_node"]
                    ros2_serv_name = ros2_node_dict["service_name"]
                    ros2_args_name = ros2_node_dict["args"]
        
                    args_name = [x.strip() for x in match.group("args").split(",")]

                    if len(args_name) != len(ros2_args_name):
                        raise Exception(f"Number of arguments does not match for action {action_name} {len(args_name)} != {len(ros2_args_name)}")

                    args = " ".join([f"{ros2_args_name[i]}=\"{args_name[i]}\"" for i in range(len(args_name))])

                    final_replecement = f"{ros2_node_name} service_name=\"{ros2_serv_name}\" {args}"
                    print(f"{action_name} -> {final_replecement}")

                    key_val = get_key_name(action_name)
                    actions_dict[key_val] = f"{ros2_node_name} service_name=\"{ros2_serv_name}\" {args} _onsuccess=\"{key_val}=DONE\""

                else:
                    raise Exception(f"Action {node} does not match the regex {RE_ACTION}")

            elif bt.nodes[node]["type"] == "WA":
                dictionary["Sequence"] = [{
                    "Wait": {get_key_name(str(node))}
                }]
                for _, child in bt.out_edges(node):
                    tmp_dict = {}
                    toXMLRec(bt, child, tmp_dict)
                    if tmp_dict:
                        dictionary["Sequence"].append(tmp_dict)

            else:
                if bt.nodes[node]["type"].lower() not in ["ee"]:
                    raise Exception("Unknown node type: {}".format(bt.nodes[node]["type"]))
                

        toXMLRec(self, self.root_name_, func_d)

        xml = dict2xml(func_d)
        # Replace the empty action tags with the correct format
        for key, val in actions_dict.items():
            xml = xml.replace(f"<{key}></{key}>", f"<{val} />")

        # Replace the delay tags with the correct format
        re_m = "<Delay>(?P<an>[^<]*)</Delay>"
        def delay_change(matchobj):
            if matchobj:
                print('test', matchobj.group('an'))
                return f"<SleepNode name=\"sleep for {matchobj.group('an')}\" msec=\"1\" _while=\"{matchobj.group('an')}!='DONE'\"/>"
            else:
                raise Exception("Error in delay_change, patter could not be matched")
        xml = re.sub(re_m, delay_change, xml)
        
        # Add the necessary initial and final tags and save to file.
        xml = XML_STANDARD.format(xml)
        if filepath:
            if pathlib.Path(filepath).parent.exists():
                with open(filepath, "w") as f:
                    f.write(xml)
            else:
                print("Filepath {} does not exist".format(filepath))
        return xml
        


def toBTfromSTNRec(bt : BehaviourTree, stn : SimpTempNet, action_id, used, level, parent) -> None:
    """
    @brief This returns a bt as a flow, that is a list of lists of nodes
    """
    action = stn.nodes(data=True)[action_id]

    # Check if the action is used
    if action in used:
        # bt.add_bt_node(BT_WAIT_ACTION(action, level, parent))
        return
    used.append(action)

    # Check if the action has children, if not, it is the end node of the BT
    action_children = stn.out_edges(action_id)
    action_parents  = stn.in_edges(action_id)
    if len(action_children) == 0:
        bt.add_bt_node(BT_EXEC_END(action, level, parent))
        return

    # Add information for the current action
    new_parent = BT_SEQ_START(action, level, parent)
    if action['type'] == "init":
        new_parent = BT_INIT_START(action, level, parent)
    bt.add_bt_node(new_parent)

    if len(action_parents) > 1:
        for p in action_parents:
            if stn.nodes(data=True)[p[0]] != parent.get_STN_node():
                bt.add_bt_node(BT_WAIT_ACTION(stn.nodes(data=True)[p[0]], level + 1, new_parent))

    if action['type'] == "start":
        bt.add_bt_node(BT_EXEC_START(action, level+1, new_parent))
    elif action['type'] == "end":
        bt.add_bt_node(BT_EXEC_END(action, level+1, new_parent))

    # Check if the children nodes should be run in parallel
    recursive_level = 0
    if len(action_children) > 1:
        new_parent = BT_PAR_START(action, level + 1, new_parent)
        bt.add_bt_node(new_parent)
        recursive_level = 1

    # Add the corresponding child(ren) to the BT
    end_node_id = None
    if action['type'] == "start":
        end_node_id = stn.getEnd(action_id)
        bt.add_bt_node(toBTfromSTNRec(bt, stn, end_node_id, used, level + recursive_level + 1, new_parent))
    for new_node_id in action_children:
        if not end_node_id or (end_node_id and end_node_id != new_node_id[1]):
            bt.add_bt_node(toBTfromSTNRec(bt, stn, new_node_id[1], used, level+recursive_level+1, new_parent))

def toBTfromSTN(bt : BehaviourTree, stn : SimpTempNet, root_name):
    # Remove negative edges
    edges_to_remove = []
    for edge in stn.edges(data=True):
        if edge[2]['weight'] < 0:
            edges_to_remove.append(edge)
    for edge in edges_to_remove:
        stn.remove_edge(edge[0], edge[1])

    return toBTfromSTNRec(bt, stn, root_name, [], 1, None)


def extract_dict(bt, node, dic):
      new_dic = {}
