build:
	jbuilder build @install --dev

test: build
	jbuilder build test/samples.exe  --dev
	jbuilder build test/docedit.exe --dev

bench: build
	jbuilder build bench/inputq.exe
	jbuilder build bench/storage.exe
	jbuilder build bench/sworker.exe
	jbuilder build bench/mworker.exe

clean:
	jbuilder clean
