from flask import Flask
from flask import jsonify
from flask_cors import CORS, cross_origin

app = Flask(__name__)
app.config['SECRET_KEY'] = 'the quick brown fox jumps over the lazy dog'
app.config['CORS_HEADERS'] = 'Content-Type'

cors = CORS(app, resources={r"/whoStarts": {"origins": "http://localhost:5000"}})
cors = CORS(app, resources={r"/getScore": {"origins": "http://localhost:5000"}})

@app.route('/whoStarts', methods=['GET','POST'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def whoStarts():
    return jsonify("1")


@app.route('/getScore', methods=['GET','POST'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def getScore():
    return jsonify("3")


