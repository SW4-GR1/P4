
all: 
		main.exe; dune exec ./main.exe test.yay

nick: main.exe;
	dune exec ./main.exe test.yay

main.exe:
		dune build main.exe

tests:
		for f in tests/*.yay; do dune exec ./main.exe $$f; done

clean:
		dune clean
		
.PHONY: all main.exe tests clean