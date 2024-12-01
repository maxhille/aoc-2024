main.js: ui/*.elm days/*.elm
	elm make ui/Main.elm --output=main.js

elm-worker.js: ui/*.elm days/*.elm
	elm make ui/Worker.elm --output=elm-worker.js
