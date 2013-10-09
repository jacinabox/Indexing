// driveletters.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <stdio.h>
#include <windows.h>

int main(int argc, char* argv[])
{
	char path[4] = "C:\\";

	for (char i='A';i<='Z';i++) {
		path[0] = i;
		if (GetDriveType(path) == DRIVE_FIXED) puts(path);
	}
	return 0;
}
