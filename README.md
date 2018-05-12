# viking-chess
What is that?

Simple web ui for viking chess, along with flask app for mocking logic.

Why?
1. Elm looks fun.
2. Someone somewhere needs something like that and, hopefully, will use it.



Prerequisites:

0. Knowing authentication token. For now it's secret for me and someone somewhere. Without it server will not respond.

1. Elm lang. https://guide.elm-lang.org/install.html
2. Python lang. https://www.python.org/downloads/ You need version >= 3.6.
3. Python packages. Navigate to project root and run:

    pip install -r python_requirements.txt

Note: if you are using Windows with virtualenv then instead of "pip install ..." you may try "python -m pip install ...". Workaround, I know, but working one.

4. Elm packages. Navigate to project root, then to elm subdirectory and run:

    elm package install

Note: it is very important to be in elm subdirectory everytime you perform operations related to elm. If you are in the root of a project some of them may even work, not all tho :( So you may end up spending a lot of time wondering what is wrong, just like me...

Running:

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
