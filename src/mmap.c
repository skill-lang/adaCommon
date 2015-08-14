/*  ___ _  ___ _ _                                                            *\
** / __| |/ (_) | |       The SKilL Generator                                 **
** \__ \ ' <| | | |__     (c) 2013-15 University of Stuttgart                 **
** |___/_|\_\_|_|____|    see LICENSE                                         **
\*                                                                            */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <string.h>

typedef struct {
   FILE const *stream;
   size_t const length;
   unsigned char const *pointer;
} mmap_read_record;

mmap_read_record error(char const *message)
{
  fprintf(stderr, "mmap.c: %s\n errno was: %s\n", message, strerror(errno));
  mmap_read_record const rval = { NULL, 0, NULL };
  return rval;
}

mmap_read_record mmap_read(char const *filename)
{
  FILE *stream = fopen(filename, "r");
  if(NULL == stream){
    fprintf(stderr, "could not open file at \"%s\"\n", filename);
    return error("Execution of function fopen failed.");
  }

  struct stat fileStat;
  if(-1 == fstat(fileno(stream), &fileStat))
    return error("Execution of function fstat failed.");

  size_t const length = fileStat.st_size;

  if(!length) {
    mmap_read_record const rval = { stream, length, NULL };
    return rval;
  }

  char const *mapped = mmap(NULL, length, PROT_READ, MAP_SHARED | MAP_POPULATE, fileno(stream), 0);

  if(MAP_FAILED == mapped)
    return error("Execution of function mmap failed.");

  mmap_read_record const rval = { stream, length, mapped };
  return rval;
}

char const* mmap_write_map(FILE *stream, size_t length, size_t offset)
{
  char const *rval = mmap(NULL, length, PROT_WRITE, MAP_SHARED | MAP_POPULATE, fileno(stream), offset);

  if(MAP_FAILED == rval)
    return NULL;

  return rval;
}

void mmap_close(FILE *stream)
{
   fclose(stream);
}
