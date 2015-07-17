.PHONY:clean debug release main valgrind

main: debug

clean:
	gprclean gnat/tester.gpr -Xmode=debug
	gprclean gnat/tester.gpr -Xmode=release

debug:
	gprbuild gnat/tester.gpr -Xmode=debug

release:
	gprbuild gnat/tester.gpr -Xmode=release

valgrind: debug
	valgrind --dsymutil=yes --leak-check=full --show-leak-kinds=all ./debug

callgrind: debug
	valgrind --tool=callgrind ./debug
