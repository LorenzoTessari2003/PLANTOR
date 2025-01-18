import os, subprocess
from flask import Flask, request, jsonify, Blueprint, current_app

from . import PUBLIC_PATH, PLOP_PATH
from ui_multi_steps import llm_scenario_comprehension, hl_llm_multi_step, ll_llm_multi_step, write_to_file, find_plan

MOCK = False


def validate_descriptions(high_level, low_level):
    """Validate compatibility of high-level and low-level descriptions."""
    current_app.logger.info("Calling validate_descriptions")
    comp, resp = llm_scenario_comprehension(high_level, low_level)
    if comp:
        return {"isValid": True}
    else:
        return {"isValid": False, "error": resp}


def generate_high_level_kb(high_level):
    """Generate the high-level knowledge base."""
    hl_d = hl_llm_multi_step(high_level)
    current_app.logger.info(f"[generate_high_level_kb] constructed\n{hl_d}")

    return hl_d


def generate_low_level_kb(low_level_desc, hl_kb):
    """Generate the low-level knowledge base."""
    ll_d = ll_llm_multi_step(low_level_desc, hl_kb)
    current_app.logger.info(f"[generate_low_level_kb] constructed\n{ll_d}")

    return ll_d


def generate_behavior_tree(kb):
    """Generate the behavior tree (BT) in XML format."""

    bt_xml_path = os.path.join(PUBLIC_PATH, "bt.xml")
    current_app.logger.info(f"[generate_behavior_tree] generating BT XML file at {bt_xml_path}")

    write_to_file(kb, os.path.join(PUBLIC_PATH, "ll_kb.pl"))

    if os.path.exists(bt_xml_path):
        os.remove(bt_xml_path)

    planner_path = os.path.join(PLOP_PATH, "python_interface", "planner.py")

    subprocess.run(["mkdir", "-p", "/app/test/test2"])

    subprocess.Popen(
        ["python3", planner_path, "-x", bt_xml_path, "-H", os.path.join(PUBLIC_PATH, "bt.html"), "-i", os.path.join(PUBLIC_PATH, "ll_kb.pl")],
        # stdout=subprocess.PIPE, 
        # stderr=subprocess.PIPE
    ).wait()

    # xml = "init"

    if os.path.exists(bt_xml_path):
        with open(bt_xml_path, "r") as f:
            xml = f.read()
    else:
        current_app.logger.error(f"[generate_behavior_tree] Error generating the behavior tree because file was not found at {bt_xml_path}.")

    if xml in ["init", ""]:
        return {"bt_error": "Error generating the behavior tree because file was not found."}

    return {"behavior_tree": xml}


app_routes = Blueprint('app_routes', __name__)

@app_routes.route('/api/validate', methods=['POST'])
def validate():
    current_app.logger.info(f"[validate] received {request}")
    if MOCK:
        return jsonify({"isValid": True})

    """Validate compatibility of high-level and low-level descriptions."""
    # current_app.logger.info(f"[validate] received {request}")
    data = request.json
    high_level = data.get('highLevel').strip()
    low_level = data.get('lowLevel').strip()

    current_app.logger.info(f"[validate] received\n{high_level}\n{low_level}")

    if not high_level or not low_level:
        return jsonify({"error": "Both fields 'highLevel' and 'lowLevel' are required."}), 400

    validation_result = validate_descriptions(high_level, low_level)

    return jsonify(validation_result)

    # if validation_result['valid']:
    #     return jsonify({"isValid": True})
    # else:
    #     return jsonify({"isValid": False, "error": validation_result['error']}), 400

@app_routes.route('/api/generate_hl_kb', methods=['POST'])
def generate_hl_kb():
    if MOCK:
        return jsonify({
            "kb": "This is the generated hl knowledge base",
            "init": "This is the generated hl initial state",
            "goal": "This is the generated hl goal state",
            "actions": "These are the generated hl actions"
        })

    """Generate the high-level knowledge base."""
    # current_app.logger.info(f"[generate_hl_kb] received {request.json}")
    data = request.json
    high_level = data.get('description').strip()

    if not high_level:
        return jsonify({"error": "Field 'description' is required."}), 400

    hl_kb = generate_high_level_kb(high_level)
    assert type(hl_kb) == dict, f"Expected dict, got {type(hl_kb)}"

    # current_app.logger.info(f"[generate_hl_kb] constructed\n{hl_kb}")
    return jsonify(hl_kb)

@app_routes.route('/api/generate_ll_kb', methods=['POST'])
def generate_ll_kb():
    if MOCK:
        return jsonify({
            'kb': "ll_kb_content",
            'init': "ll_init_content",
            'goal': "ll_goal_content",
            'actions': "ll_actions_content",
            'mappings': "ll_mappings_content",
        })

    """Generate the low-level knowledge base."""
    # current_app.logger.info(f"[generate_ll_kb] received {request.json}")
    data = request.json
    low_level_desc = data.get('lowLevelDesc').strip()
    hl_kb_content = data.get('hlkbContent').strip()
    hl_init_content = data.get('hlInitContent').strip()
    hl_goal_content = data.get('hlGoalContent').strip()
    hl_actions_content = data.get('hlActionsContent').strip()

    if not all([low_level_desc, hl_kb_content, hl_init_content, hl_goal_content, hl_actions_content]):
        return jsonify({"error": "Field 'low_level_desc' is required."}), 400

    hl_d = {
        'kb': hl_kb_content,
        'init': hl_init_content,
        'goal': hl_goal_content,
        'actions': hl_actions_content
    }

    ll_kb = generate_low_level_kb(low_level_desc, hl_d)
    # current_app.logger.info(f"[generate_ll_kb] constructed\n{ll_kb}")
    assert type(ll_kb) == dict, f"Expected dict, got {type(ll_kb)}"
    return jsonify(ll_kb)

@app_routes.route('/api/generate_bt', methods=['POST'])
def generate_bt():
    if MOCK:
        return jsonify({"behavior_tree": "This is a BT"})

    """Generate the behavior tree (BT) in XML format."""
    current_app.logger.info(f"[generate_bt] received {request.json}")
    data = request.json
    low_level_kb = data.get('low_level_kb')
    low_level_init = data.get('low_level_init')
    low_level_goal = data.get('low_level_goal')
    low_level_actions = data.get('low_level_actions')
    low_level_mappings = data.get('low_level_mappings')

    if not low_level_kb or not low_level_init or not low_level_goal or not low_level_actions or not low_level_mappings:
        return jsonify({"error": "All fields `low-level_{kb,init,goal,actions,mappings}` are required."}), 400

    kb = {
        'kb': low_level_kb,
        'init': low_level_init,
        'goal': low_level_goal,
        'actions': low_level_actions,
        'mappings': low_level_mappings
    }

    bt_xml = generate_behavior_tree(kb)

    assert type(bt_xml) == dict, f"Expected dict, got {type(bt_xml)}"
    if bt_xml != "":
        return jsonify(bt_xml) # Request was successful but there was an error generating the BT

    else:
        current_app.logger.info(f"[generate_bt] constructed\n{bt_xml}")
        return jsonify(bt_xml)
