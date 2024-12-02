import pyswip


from . import PLANNER_PATH


def extract_actions(actions) -> list:
    """
    Actions have the form ID-Action(Args), which, when obtained by PySWI, becomes
    -(ID, Action), where ID and Action are part of the args member of Functor. 
    
    Args:
        actions (list): A list of Functors containing the actions.

    Returns:
        list: A list of strings containing the actions.
    """
    ret = list()
    for i in range(len(actions)):
        action = f"{actions[i].args[0]}_{actions[i].args[1]}"

        ret.append(action)
    return ret


###############################################################################


def extract_resources(resources) -> dict:
    """
    Resource have the form N-Resource, which, when obtained by PySWI, become
    -(N, Resource), so we need to extract these values as a dictionary where 
    Resource is the key and N is the value. 

    Args:
        resources (list): A list of Functors containing the resources.

    Returns:
        dict: A dictionary containing the resources as keys and how many as values.
    """
    ret = dict()
    for i in range(len(resources)):
        resource = str(resources[i].args[0])
        if resource not in ret:
            ret[resource] = int(str(resources[i].args[1]))
        else:
            raise ValueError(f"Resource {resource} is duplicated")
    return ret


###############################################################################


def extract_resources_list(resources_list) -> list:
    """
    Resources have the form ResourceName-ResourceList, which, when obtained by 
    PySWI, becomes -(ResourceName, ResourceList), so we need to extract these 
    values as a list of strings.

    Args:
        resources_list (list): A list of Functors containing the resources.

    Returns:
        dict: A dictionary where the key is the resource name and the value
            is the list of resources. For examples:
            ret['agent']=['agent(a1)', 'agent(a2)'].
    """
    ret = {}
    for i in range(len(resources_list)):
        resource_name = str(resources_list[i].args[0])
        resource_list = [str(x) for x in resources_list[i].args[1]]
        ret[resource_name] = resource_list
    return ret


###############################################################################


def extract_ll_actions_list(ll_actions_list) -> list:
    """
    Extracts the low-level actions from the list of low-level actions.

    Args:
        ll_actions_list (list): A list of Atoms containing the low-level actions.

    Returns:
        list: A list of strings containing the low-level actions.
    """
    return [str(x) for x in ll_actions_list]


###############################################################################


def extract_tt_actions(tt_actions, res_x_action) -> dict:
    """
    Time Triggered Actions have the form ID-[IDStart-Start]-[IDEnd-End], which, 
    when obtained by PySWI, becomes 
    -(ID, TTA, Args), where ID, TTA and Args are all part of the args
    member of Functor. So we need to extract these values. 
    """
    res_x_action_dict = dict()
    for i in range(len(res_x_action)):
        key = str(res_x_action[i].args[0])
        value = [str(x) for x in res_x_action[i].args[1]]
        res_x_action_dict[key] = value

    ret = dict()
    for i in range(len(tt_actions)):
        tt_action_id = str(tt_actions[i].args[0].args[0])

        start_id = str(tt_actions[i].args[0].args[1][0].args[0])
        start_action = str(tt_actions[i].args[0].args[1][0].args[1])

        end_id = str(tt_actions[i].args[1][0].args[0])
        end_action = str(tt_actions[i].args[1][0].args[1])

        ret[tt_action_id] = {
            "s": f"{start_id}_{start_action}",
            "e": f"{end_id}_{end_action}",
            "l": 10 if tt_action_id != "0" else 0,
            "u": 1000 if tt_action_id != "0" else int(1e12),
            "R": res_x_action_dict[tt_action_id] if tt_action_id in res_x_action_dict else []
        }

    return ret


###############################################################################


def execTest(query = "plan", kb_path = "") -> dict:
    """
    Looks for the planner in the prolog_planner directory and executes the query specified in the argument.

    Args:
        query (str): The query to be executed. Defaults to "plan/5".
    
    Returns:
        dict: A dictionary containing the snap actions ('actions'), 
            the time triggered actions ('tt_actions'), the adjacency matrix ('adj_matrix'), 
            the list of resources ('resources'), 
            and the dictionary of resources used per action ('res_x_actions').
    """
    print(f"Executing {query} from Prolog")
    
    prolog = pyswip.Prolog()
    # planner_path = os.path.join(os.getcwd(), "..", "prolog_planner")
    print(f"Looking for planner at {PLANNER_PATH}")
    if kb_path != "":
        prolog.consult(kb_path)
    prolog.consult(PLANNER_PATH)

    planner = pyswip.Functor(query, 7)
    actions_var = pyswip.Variable()
    tt_actions_var = pyswip.Variable()
    adj_matrix_var = pyswip.Variable()
    resources_var = pyswip.Variable()
    res_x_actions_var = pyswip.Variable()
    resources_list = pyswip.Variable()
    ll_actions_list_var = pyswip.Variable()

    sol = pyswip.Query(planner(actions_var, adj_matrix_var, tt_actions_var, resources_var, res_x_actions_var, resources_list, ll_actions_list_var))

    succ = sol.nextSolution()

    if succ == 0:
        raise ValueError("Failed to execute prolog")
    
    print("Executed prolog")

    # Convert the following code into a dictionary 
    data_dict = {
        "actions":          extract_actions(actions_var.get_value()),
        "tt_actions":       extract_tt_actions(tt_actions_var.get_value(), res_x_actions_var.get_value()),
        "adj_matrix":       adj_matrix_var.get_value(),
        "resources":        extract_resources(resources_var.get_value()),
        "resources_list":   extract_resources_list(resources_list.get_value()),
        "ll_actions_list":  extract_ll_actions_list(ll_actions_list_var.get_value())
    }

    data_dict["actions"].insert(0, data_dict["tt_actions"]["0"]["s"])
    data_dict["actions"].append(data_dict["tt_actions"]["0"]["e"])

    # for key, value in data_dict.items():
    #     print(key, value)
    #     print()

    return data_dict


