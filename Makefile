server:
	cd resources/public && python -m SimpleHTTPServer

upload:
	rsync -av resources/public/ www-data@atomsk.procrustes.net:~/ld34.procrustes.net/public_html/

archive:
	cd resources/public && tar cvzf ../../ld34.procrustes.net.tar.gz .

resources/public/img/sprites.png: src/gfx/sprites.png
	-mkdir resources/public/img/
	convert src/gfx/sprites.png -alpha On -transparent '#a2d000' resources/public/img/sprites.png

images: resources/public/img/sprites.png

sfx:
	cd src/sfx && oggenc *.wav

	-mkdir resources/public/sfx/
	cp src/sfx/*.ogg resources/public/sfx/
