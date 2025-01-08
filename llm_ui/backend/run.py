# run.py

from flask import Flask
from flask_cors import CORS
from app.routes import app_routes

app = Flask(__name__)
CORS(app)

# Register blueprints
app.register_blueprint(app_routes)

@app.route('/print1')
def print1():
    return "Hello World"

if __name__ == "__main__":
    app.run(debug=True, host="0.0.0.0", port=5000)
