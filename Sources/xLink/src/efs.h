
#ifndef EFS_H
#define EFS_H

/*----------------------------------------------------------------------------*/
/*                        Embedded File System                                */
/*----------------------------------------------------------------------------*/

extern void ReadEmbeddedFS (const char * efsDescFile, const char * dataFileName);
extern void SerializeEmbeddedFS (const char * efsDescFile, const char * dataFileName);
extern void FormEmbeddedFS ();

#endif // EFS_H
