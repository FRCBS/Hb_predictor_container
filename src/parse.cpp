#include <iostream>
#include <fstream>
#include <string>
#include <regex>
#include <cassert>
#include <sstream>
//#include <filesystem>

// Multipart encoding is decribed here:
// https://www.ietf.org/rfc/rfc2046.txt
// https://www.ietf.org/rfc/rfc2183.txt

bool
mygetline(std::fstream&s, std::string& line)
{
  if (not std::getline(s, line))
    return false;
  if (line.back() == 0xd)
	line.pop_back();
  return true;
}


char*
nextline(char* begin, char* end)
{
  for (char* p=begin; p < end-1; ++p) {
    if (*p == 0xd and *(p+1) == 0xa)
      return p+2;
  }
  return nullptr;
}

std::string
save_file(char* begin, int param_size)
{
  char filename[] = "/tmp/jarkkoXXXXXX";
  int fd = mkstemp(filename);
  if (fd == -1) {
    fprintf(stderr, "Could not create a temporary file\n");
    exit(1);
  }
  FILE* f = fdopen(fd, "w");
  if (f == NULL) {
    fprintf(stderr, "Could not create a temporary file\n");
    exit(1);
  }
  size_t n = fwrite(begin, 1, param_size, f);
  fclose(f);
  printf("Wrote %zu bytes\n", n);
  return filename;
}

// Escape the double quotation marks
std::string
myencode(std::string s)
{
  std::string result;
  for (char c : s) {
    if (c == '"')
      result.append("\\\"");
    else if (c == '\n')
      result.append("\\n");
    else if (c == '\r')
      result.append("\\r");
    else
      result.push_back(c);
  }
  return result;
}

char*
get_next_parameter(char* begin, char* end, std::string separator,
		   std::string& result, FILE* f, int counter)
{
  separator = std::string("\x0d\x0a") + separator;
  std::string name;
  std::string filename;
  std::string content_type;
  bool is_file = false;
  char* current = begin;
  
  char* next = nextline(current, end);
  std::string line(current, next - current - 2);
  current = next;
  if (line == "--") 
    return end;
  else
    assert(line == "");
 
  next = nextline(current, end);
  line = std::string(current, next - current - 2);
  current = next;

  std::smatch base_match;
  std::regex re1("Content-Disposition: form-data; name=\"(.*)\"; filename=\"(.*)\"");
  std::regex re2("Content-Disposition: form-data; name=\"(.*)\"");
  std::string head;
  if (std::regex_match(line, base_match, re1)) {
    name = base_match[1];
    filename = base_match[2];
    head = line;
    
    next = nextline(current, end);
    line = std::string(current, next - current - 2);
    current = next;
    
    std::regex re3("Content-Type: (.*)");
    std::regex_match(line, base_match, re3);
    head = myencode(head + "\x0d\x0a" + line);
    content_type = base_match[1];
    is_file = true;
  } else if (std::regex_match(line, base_match, re2)) {
    name = base_match[1];
  } else {
    fprintf(stderr, "Severe error\n");
    exit(1);
  }

  next = nextline(current, end);
  line = std::string(current, next - current - 2);
  current = next;

  assert(line == "");
  while (true) {
    char* it = search(current, end, separator.begin(), separator.end());
    if (it == end) { // Not found
      return end;
    } else {
      int param_size = it - current;      
      if (counter > 0 and f != nullptr)
	fprintf(f, ",\n");
      if (is_file) {
	std::string temp_filename = save_file(current, param_size);
	printf("Wrote to file %s\n", temp_filename.c_str());
	printf("name: %s; size: %i; filename: %s; content_type: %s\n",
	       name.c_str(), param_size, filename.c_str(), content_type.c_str());
	fprintf(f, "\"%s\" : { \"filename\":\"%s\", \"tempfile\":\"%s\", \"content_type\":\"%s\", \"head\":\"%s\" }", 
		name.c_str(), filename.c_str(), temp_filename.c_str(), content_type.c_str(), head.c_str());
      } else {
	std::string param(current, param_size);
	printf("name: %s; size: %i\n", name.c_str(), param_size);
	if (f)
	  fprintf(f, "\"%s\" : \"%s\"", name.c_str(), param.c_str());
      }
      return it + separator.length();
    }
  }
}


int
main(int argc, char* argv[])
{
  if (argc < 2) {
    fprintf(stderr, "Give at least one parameter\n");
    exit(1);
  }
  std::string separator;
  std::string filename = argv[1];
  std::string json_filename;
  if (argc >= 3) {
    separator = argv[2];
  }
  FILE* f = nullptr;
  if (argc >= 4) {
    json_filename = argv[3];
  }
  
  std::fstream s(filename, s.binary | s.in | s.ate);
  if (!s.is_open()) {
    printf("failed to open %s\n", filename.c_str());
  } else {
    //int size = std::filesystem::file_size(filename);
    // Get the file size
    std::streamsize size = s.tellg();
    s.seekg(0, std::ios::beg);
    //std::vector<char> buffer(size);
    // Read the whole file
    std::string buffer(size, 0);
    char* begin = buffer.data();
    char* end = buffer.data() + size;
    if (not s.read(begin, size))
    {
      fprintf(stderr, "Error\n");
      exit(1);
    }

    printf("Managed to open %s\n", filename.c_str());
    if (json_filename.length())
      f = fopen(json_filename.c_str(), "w");
    char* current = begin;

    /*
    if (separator.length() == 0) {
      // Read the header
      while (true) {
	char* next = nextline(current, end);
	//printf("Read line\n");
	int line_length = next - current - 2;
	if (line_length == 0) {  // empty line
	  current = next;
	  break;
	}
	std::regex re("Content-Type: multipart/form-data; boundary=(-+[0-9]+)");
	std::smatch base_match;
	//std::basic_string_view line(current, line_length);
	std::string line(current, line_length);
	if (std::regex_match(line, base_match, re)) {
	  separator = base_match[1];
	  printf("Separator is %s\n", separator.c_str());
	  separator = std::string("--") + separator;
	}
	std::cout << line << std::endl;
	current = next;
      }
    }
    */
    char* next = nextline(current, end);
    std::string line(current, next - current - 2);
    printf("The separator is %s\n", separator.c_str());
    if (separator.length() == 0)
      separator = line;
    if (line != separator) {
      printf("Got %s instead of separator line\n", line.c_str());
      exit(1);
    }
    std::string result;
    next -= 2;
    // Read the parts from the multipart body
    if (f)
      fprintf(f, "{\n");

    int counter=0;
    while (true) {
      next = get_next_parameter(next, end, separator, result, f, counter);
      ++counter;
      if (next == end)
	break;
    }
    if (f) {
      fprintf(f, "\n}\n");
      fclose(f);
    }
  }
  return 0;
}


