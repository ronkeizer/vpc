# build md into html in _book folder
gitbook build

# for some reason Heroku doesn't like the _book folder...
rm -rf public
cp -R _book public
