import logging 
from flask import Flask, request, jsonify, Blueprint, current_app
# from validation import validate_descriptions
# from hl_generator import generate_high_level_kb
# from ll_generator import generate_low_level_kb
# from bt_generator import generate_behavior_tree

def validate_descriptions(high_level, low_level):
    """Validate compatibility of high-level and low-level descriptions."""
    # Placeholder implementation
    return {"valid": True}

def generate_high_level_kb(high_level):
    """Generate the high-level knowledge base."""
    # Placeholder implementation
    return "This is the HL kb"

def generate_low_level_kb(low_level, high_level_kb):
    """Generate the low-level knowledge base."""
    # Placeholder implementation
    return "This is the LL kb"

def generate_behavior_tree(high_level_kb, low_level_kb):
    """Generate the behavior tree (BT) in XML format."""
    # Placeholder implementation
    return "Behavior tree"

app_routes = Blueprint('app_routes', __name__)

@app_routes.route('/api/validate', methods=['POST'])
def validate():
    """Validate compatibility of high-level and low-level descriptions."""
    current_app.logger.info(f"[validate] received {request}")
    data = request.json
    high_level = data.get('highLevel')
    low_level = data.get('lowLevel')

    if not high_level or not low_level:
        return jsonify({"error": "Both fields 'highLevel' and 'lowLevel' are required."}), 400

    validation_result = validate_descriptions(high_level, low_level)

    if validation_result['valid']:
        return jsonify({"isValid": True})
    else:
        return jsonify({"isValid": False, "error": validation_result['error']}), 400

@app_routes.route('/api/generate_hl_kb', methods=['POST'])
def generate_hl_kb():
    """Generate the high-level knowledge base."""
    current_app.logger.info(f"[generate_hl_kb] received {request.json}")
    data = request.json
    high_level = data.get('description')

    if not high_level:
        return jsonify({"error": "Field 'description' is required."}), 400

    hl_kb = generate_high_level_kb(high_level)
    return jsonify({"kb": hl_kb})

@app_routes.route('/api/generate_ll_kb', methods=['POST'])
def generate_ll_kb():
    """Generate the low-level knowledge base."""
    current_app.logger.info(f"[generate_ll_kb] received {request.json}")
    data = request.json
    low_level_desc = data.get('lowLevelDesc')
    high_level_kb = data.get('highLevelKB')

    if not low_level_desc or not high_level_kb:
        return jsonify({"error": "Both fields 'low_level_desc' and 'high_level_kb' are required."}), 400

    ll_kb = generate_low_level_kb(low_level_desc, high_level_kb)
    return jsonify({"kb": ll_kb})

@app_routes.route('/api/generate_bt', methods=['POST'])
def generate_bt():
    """Generate the behavior tree (BT) in XML format."""
    current_app.logger.info(f"[generate_bt] received {request.json}")
    data = request.json
    high_level_kb = data.get('high_level_kb')
    low_level_kb = data.get('low_level_kb')

    if not high_level_kb or not low_level_kb:
        return jsonify({"error": "Both high-level and low-level knowledge bases are required."}), 400

    bt_xml = generate_behavior_tree(high_level_kb, low_level_kb)
    # return jsonify({"bt_error": "Could not plan because"}) # Request was successful but there was an error generating the BT
    return jsonify({"behavior_tree": bt_xml})
