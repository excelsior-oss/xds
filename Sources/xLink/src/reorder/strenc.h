#ifndef _STRENC_H_
#define _STRENC_H_

/*----------------------------------------------------------------------------*/
/*                   Zero-terminated strings encryption                       */
/*----------------------------------------------------------------------------*/

#define IMPORT_EXPORT_ENCRYPTION_KEY 99 /* used for import&export encryption */

inline unsigned char xorEx(unsigned char b, unsigned char key) {
    if (b == 0) {
        return 0;
    } else if (b == key) {
        return b;
    } else {
        return b^key;
    }
}

inline unsigned char decryptByte(unsigned char b, unsigned char key) {
    return xorEx(b, key);
}

inline unsigned char encryptByte(unsigned char b, unsigned char key) {
    return xorEx(b, key);
}

//  decrypts data from src coping it to dst
//  dst must point to buffer that large enough to contain len data
//  dst and src may be equal
inline void decryptStr(const char * src, char * dst, int len, unsigned char key) {
    for (int i = 0; i < len; i++) {
        dst[i] = decryptByte(src[i], key);
    }        
}

//  encrypts data from src coping it to dst, saving zero
//  dst must point to buffer that large enough to contain len data
//  dst and src may be equal
inline void encryptData(const char * src, char * dst, int len, unsigned char key) {
    for (int i = 0; i < len; i++) {
        dst[i] = src[i] ^ key;
    }        
}

#endif // _STRENC_H_