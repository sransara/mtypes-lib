build:
	jbuilder build @install

test: build
	jbuilder build test/samples.exe 
	jbuilder build test/docedit.exe

bench: build
	jbuilder build bench/inputq.exe --dev
	jbuilder build bench/storage.exe --dev
	jbuilder build bench/sworker.exe --dev

clean:
	jbuilder clean
