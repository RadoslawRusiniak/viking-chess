from flask import Flask
from flask import jsonify
from flask_cors import CORS, cross_origin

app = Flask(__name__)
app.config['SECRET_KEY'] = 'the quick brown fox jumps over the lazy dog'
app.config['CORS_HEADERS'] = 'Content-Type'

cors = CORS(app, resources={
    r"/getScore": {"origins": "http://localhost:5000"}
    , r"/getBoard": {"origins": "http://localhost:5000"}
})


@app.route('/getScore', methods=['GET','POST'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def getScore():
    return jsonify("3")


@app.route('/getBoard', methods=['GET','POST'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def getBoard():
    return jsonify(
        ["...aaaaa..."
        ,".....a....."
        ,"..........."
        ,"a....d....a"
        ,"a...ddd...a"
        ,"aa.ddkdd.aa"
        ,"a...ddd...a"
        ,"a....d....a"
        ,"..........."
        ,".....a....."
        ,"...aaaaa..."]
        )

@app.route('/getMoves', methods=['GET','POST'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def getMoves():
    return jsonify(
        [ [1,1]
        , [1,2]
        , [2,2] ]
        )
