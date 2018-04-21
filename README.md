# viking-chess
What's that?
Simple web ui for viking chess, along with flask app for mocking logic.

Why?
1. Elm looks fun.
2. Someone somewhere needs something like that and, hopefully, will use it.



Prerequisites:
1. Elm lang. https://guide.elm-lang.org/install.html
2. Python lang. https://www.python.org/downloads/
3. Flask. After Python is installed run:
  pip install flask

Running:
0. (Before first run) Install elm packages:
  0.1. Navigate to elm subdirectory:
    cd elm
  0.2. Install packages:
    elm package install

1. Run elm:
  1.1. Navigate to elm subdirectory:
    cd elm
  1.2. Run elm:
    elm-reactor --port 8123

2. Run flask:
  2.1. Go to flask subdirectory:
    cd flask
  2.2. Run:
    a) on Linux:
      FLASK_APP=main.py flask run --port=5000
    b) on Windows:
      $env:FLASK_APP="main.py"
      flask run --port=5000
  Important: do not change flask port (for now it's hardcoded to 5000 in app)
