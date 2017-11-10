build:
	jbuilder build @install --dev

test: build
	jbuilder build test/samples.exe
	jbuilder build test/docedit.exe
	jbuilder build test/taskmaster.exe

clean:
	jbuilder clean
