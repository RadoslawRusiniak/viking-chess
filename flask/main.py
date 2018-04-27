from flask import Flask
from flask import jsonify
from flask_cors import CORS, cross_origin

app = Flask(__name__)
app.config['SECRET_KEY'] = 'the quick brown fox jumps over the lazy dog'
app.config['CORS_HEADERS'] = 'Content-Type'

cors = CORS(app, resources={
    r"/getScore": {"origins": "http://localhost:5000"}
    , r"/getHistory": {"origins": "http://localhost:5000"}
    , r"/getReachablePositions": {"origins": "http://localhost:5000"}
    , r"/makeMove": {"origins": "http://localhost:5000"}
    , r"/getHint": {"origins": "http://localhost:5000"}
})


@app.route('/getScore', methods=['GET','POST'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def getScore():
    return jsonify(
    {
        "score": 3
    })


@app.route('/getHistory', methods=['GET','POST'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def getBoard():
    return jsonify(
        {
            "history":
            [
                {
                    "board":
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
                },
                {
                    "board":
                        ["...aaaaa..."
                        ,".....a....."
                        ,"..........."
                        ,"a....d....a"
                        ,"..a.ddd...a"
                        ,"aa.ddkdd.aa"
                        ,"a...ddd...a"
                        ,"a....d....a"
                        ,"..........."
                        ,".....a....."
                        ,"...aaaaa..."]
                }
            ]
        })

@app.route('/getReachablePositions', methods=['GET','POST'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def getMoves():
    return jsonify(
    {
        "positions": [{
            "row": 1,
            "column": 1
        }, {
            "row": 1,
            "column": 2
        }]
    })

@app.route('/makeMove', methods=['GET','POST'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def makeMove():
    return jsonify(
        {
            "board":    
                ["...aaaaa..."
                ,"..a........"
                ,"..........."
                ,"a....d....a"
                ,"a...ddd...a"
                ,"aa.ddkdd.aa"
                ,"a...ddd...a"
                ,"a....d....a"
                ,"..........."
                ,".....a....."
                ,"...aaaaa..."]
        })

@app.route('/getHint', methods=['GET','POST'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def getHint():
    return jsonify(
    {
        "hint": {
            "from": {
                "row": 5,
                "column": 5
            },
            "to": {
                "row": 2,
                "column": 5
            }
        }
    })