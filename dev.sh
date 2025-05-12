elm make src/Main.elm --output=elm.js
npx tailwindcss -i src/input.css -o tailwind.build.css --minify
open index.html