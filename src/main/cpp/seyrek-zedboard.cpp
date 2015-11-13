#include "sdcard.h"
#include <string>

// Seyrek platform function implementations for the ZedBoard


void * readMatrixData(std::string name, std::string component) {
  mount();
  std::string matricesBase = "";
  std::string fileName = matricesBase + "/" + name + "/" + name + "-" + component + ".bin";
  unsigned int fsize = getFileSize(fileName.c_str());

  if(!fsize) throw "Could not get file size";

  void * buf = new char[fsize];
  readFromSDCard(fileName.c_str(), (unsigned int) buf);
  unmount();

  return buf;
}
