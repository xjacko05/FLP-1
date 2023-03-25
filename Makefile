all:
	ghc -Wall main.hs -o flp22-fun

.PHONY: test
test:
	./test.sh

	rm  ./tests/*.in ./tests/*.out ./tests/*.real

.PHONY: clean
clean:
	rm ./flp22-fun ./*.o ./*.hi