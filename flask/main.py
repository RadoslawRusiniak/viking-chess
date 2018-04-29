from flask import Flask
from flask import jsonify
from flask import request
from flask_cors import CORS, cross_origin

from passlib.hash import sha256_crypt

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

hashed = "$5$rounds=535000$ENk3T25VRNpieMOa$nEnASPxPNmdar6.a9ETs6rSaNbRRTtPSpbqpnIMpqRA"
def isCorrectToken(hdrs):
    token = hdrs['authenticationToken']
    return sha256_crypt.verify(token, hashed)

@app.route('/getScore', methods=['GET'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def getScore():
    if (not isCorrectToken(request.headers)):
        return None #TODO some error here

    return jsonify(
    {
        "score": 3
    })


@app.route('/getHistory', methods=['GET'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def getBoard():
    if (not isCorrectToken(request.headers)):
        return None #TODO some error here

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
                ,
                "whoMoves": 1
            },
            {
                "board":
                    ["...aaaaa..."
                    ,".....a....."
                    ,"..........."
                    ,"..a..d....a"
                    ,"a...ddd...a"
                    ,"aa.ddkdd.aa"
                    ,"a...ddd...a"
                    ,"a....d....a"
                    ,"..........."
                    ,".....a....."
                    ,"...aaaaa..."]
                ,
                "whoMoves": 2
            }
        ]
    })

@app.route('/getReachablePositions', methods=['GET'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def getReachablePositions():
    if (not isCorrectToken(request.headers)):
        return None #TODO some error here

    return jsonify(
    {
        "positions": [{
            "row": 1,
            "column": 7
        }, {
            "row": 2,
            "column": 7
        }, {
            "row": 3,
            "column": 7
        }, {
            "row": 4,
            "column": 7
        }, {
            "row": 6,
            "column": 7
        }, {
            "row": 7,
            "column": 7
        }, {
            "row": 8,
            "column": 7
        }, {
            "row": 9,
            "column": 7
        }, {
            "row": 5,
            "column": 8
        }]
    })

@app.route('/makeMove', methods=['GET'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def makeMove():
    if (not isCorrectToken(request.headers)):
        return None #TODO some error here

    return jsonify(
    {
        "board":    
            ["...aaaaa..."
            ,".....a....."
            ,".......d..."
            ,"..a..d....a"
            ,"a...ddd...a"
            ,"aa.ddkd..aa"
            ,"a...ddd...a"
            ,"a....d....a"
            ,"..........."
            ,".....a....."
            ,"...aaaaa..."]
        ,
        "whoMoves": 1
    })

@app.route('/getHint', methods=['GET'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def getHint():
    if (not isCorrectToken(request.headers)):
        return None #TODO some error here

    return jsonify(
    {
        "hint": {
            "from": {
                "row": 5,
                "column": 7
            },
            "to": {
                "row": 2,
                "column": 7
            }
        }
    })