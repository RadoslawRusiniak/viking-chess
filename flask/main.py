from flask import Flask
from flask import jsonify
from flask import request
from flask_cors import CORS, cross_origin

import json
import secrets

from passlib.hash import sha256_crypt

from betafl import FetlarEngine
from betafl.elements.move import Coordinates, Move

e = FetlarEngine(2)

app = Flask(__name__)
app.config['SECRET_KEY'] = 'the quick brown fox jumps over the lazy dog'
app.config['CORS_HEADERS'] = 'Content-Type'

cors = CORS(app, resources={
    r"/initGame": {"origins": "http://localhost:5000"}
    , r"/getScore": {"origins": "http://localhost:5000"}
    , r"/getHistory": {"origins": "http://localhost:5000"}
    , r"/getReachablePositions": {"origins": "http://localhost:5000"}
    , r"/makeMove": {"origins": "http://localhost:5000"}
    , r"/getHint": {"origins": "http://localhost:5000"}
})

hashedPassword = "$5$rounds=535000$CEoVj1v120WFeYfX$027jiw66sPzekyGfxq.ptuCOs6LyR8h4UjI9IJf3EV2"
def isCorrectPassword(hdrs):
    password = hdrs.get('password')
    return sha256_crypt.verify(password, hashedPassword)

generatedToken = secrets.token_urlsafe()
def isCorrectRequest(hdrs):
    requestToken = hdrs.get('authenticationToken')
    return requestToken == generatedToken

def parseCoordinates(data):
    parsed = json.loads(data)
    row, col = parsed["location"]["row"], parsed["location"]["column"]
    return Coordinates(row, col)

@app.route('/initGame', methods=['GET'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def initGame():
    if (not isCorrectPassword(request.headers)):
        return None #TODO some error here

    e.new_game()

    return jsonify(
    {
        "token": generatedToken,
        "history":
        [
            {
                "board": e.board.to_string(),
                "whoMoves": e.current_side
            }
        ]
    })


@app.route('/getScore', methods=['GET'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def getScore():
    if (not isCorrectRequest(request.headers)):
        return None #TODO some error here

    return jsonify(
    {
        "score": 3
    })


@app.route('/getHistory', methods=['GET'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def getBoard():
    if (not isCorrectRequest(request.headers)):
        return None #TODO some error here

    return jsonify(
    {
        "history":
        [
            {
                "board": "...aaaaa........a................a....d....aa...ddd...aaa.ddkdd.aaa...ddd...aa....d....a................a........aaaaa...",
                "whoMoves": 0
            },
            {
                "board": "...aaaaa........a..................a..d....aa...ddd...aaa.ddkdd.aaa...ddd...aa....d....a................a........aaaaa...",
                "whoMoves": 1
            }
        ]
    })

@app.route('/getReachablePositions', methods=['GET'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def getReachablePositions():
    if (not isCorrectRequest(request.headers)):
        return None #TODO some error here

    gameLoc = parseCoordinates(request.args["location"])

    positions = e.board.get_reachable_from_position(gameLoc)

    jsonPositions = []
    for position in positions:
        jsonPos = {
            "row": position.row,
            "column": position.column
        }
        jsonPositions.append(jsonPos)

    return jsonify(
    {
        "positions": jsonPositions
    })

@app.route('/makeMove', methods=['GET'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def makeMove():
    if (not isCorrectRequest(request.headers)):
        return None #TODO some error here

    locFrom = parseCoordinates(request.args["from"])
    locTo = parseCoordinates(request.args["to"])

    e.make_move(Move(locFrom, locTo).to_string())
    
    return jsonify(
    {
        "board": e.board.to_string(),
        "whoMoves": e.current_side
    })

@app.route('/getHint', methods=['GET'])
@cross_origin(origin='localhost',headers=['Content-Type','Authorization'])
def getHint():
    if (not isCorrectRequest(request.headers)):
        return None #TODO some error here

    hint = e.go()

    return jsonify(
    {
        "hint": {
            "from": {
                "row": hint.coord_from.row,
                "column": hint.coord_from.column
            },
            "to": {
                "row": hint.coord_to.row,
                "column": hint.coord_to.column
            }
        }
    })