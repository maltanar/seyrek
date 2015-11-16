#include <string>
#include <stdio.h>

void * readMatrixData(std::string name, std::string component) {
  std::string matricesBase = "/home/maltanar/seyrek/matrices";
  std::string fileName = matricesBase + "/" + name + "/" + name + "-" + component + ".bin";
  FILE *f = fopen(fileName.c_str(), "rb");
  if(!f) throw (std::string("Could not open file: ") + fileName).c_str();
  fseek(f, 0, SEEK_END);
  unsigned int fsize = ftell(f);
  fseek(f, 0, SEEK_SET);

  void * buf = new char[fsize];
  unsigned int r = fread(buf, 1, fsize, f);

  if(r != fsize) throw "Read error";

  fclose(f);

  return buf;
}
