build:
	jbuilder build @install --dev

test: build
	jbuilder build test/samples.exe
	jbuilder build test/docedit.exe

bench: build
	jbuilder build bench/inputq.exe
	jbuilder build bench/storage.exe
	jbuilder build bench/sworker.exe
	jbuilder build bench/mworker.exe
	jbuilder build bench/mtest.exe

clean:
	jbuilder clean
