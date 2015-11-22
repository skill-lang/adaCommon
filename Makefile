.PHONY:clean debug release skill_common main valgrind leakcheck callgrind

main: debug

clean:
	gprclean gnat/tester.gpr -Xmode=debug
	gprclean gnat/tester.gpr -Xmode=release
	-rm -f lib/Debug/*
	-rm -f lib/Release/*
	-rm -f obj/Debug/*
	-rm -f obj/Release/*

debug:
	gprbuild gnat/tester.gpr -Xmode=debug

release:
	gprbuild gnat/tester.gpr -Xmode=release

skill_common:
	gprbuild gnat/skill_common.gpr -Xmode=release

valgrind: debug
	valgrind --dsymutil=yes --leak-check=full --show-leak-kinds=all ./debug

leakcheck: debug
	valgrind --dsymutil=yes --leak-check=full --show-leak-kinds=definite --errors-for-leak-kinds=definite ./debug

callgrind: debug
	valgrind --tool=callgrind ./debug
