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
#include <unistd.h>

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

  void *mapped = mmap(NULL, length, PROT_READ, MAP_SHARED, fileno(stream), 0);

  if(MAP_FAILED == mapped)
    return error("Execution of function mmap failed.");

  if (-1 == posix_madvise(mapped, length, MADV_WILLNEED))
    return error("Execution of function madvise failed.");

  mmap_read_record const rval = { stream, length, mapped };
  return rval;
}

char const* mmap_write_map_block(FILE *stream, size_t length)
{
  // advance file position
  fseek(stream, length, SEEK_CUR);

  // create map
  void *rval = mmap(NULL, ftell(stream), PROT_READ | PROT_WRITE, MAP_SHARED, fileno(stream), 0);

  if(MAP_FAILED == rval){
    fprintf(stderr, "mmap.c: %s\n errno was: %s\n", "failed to create write map", strerror(errno));
    return NULL;
  }

  if (-1 == posix_madvise(rval, length, MADV_SEQUENTIAL | MADV_WILLNEED)){
    fprintf(stderr, "mmap.c: %s\n errno was: %s\n", "failed to advise write map", strerror(errno));
    return NULL;
  }

  // resize file
  ftruncate(fileno(stream), ftell(stream));

  return rval;
}

void mmap_write_unmap(void *base, void *eof)
{

  // release the map
  munmap(base, eof - base);
}

void mmap_close(FILE *stream)
{
   fclose(stream);
}
