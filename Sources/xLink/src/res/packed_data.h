
#ifdef __cplusplus
extern "C" {
#endif

typedef struct PackedFile {
    char* name;
    unsigned char* compressed_data;
    unsigned long compressed_len;
    unsigned long uncompressed_len;
} PackedFile;


extern PackedFile PackedData[];

#ifdef __cplusplus
};
#endif
