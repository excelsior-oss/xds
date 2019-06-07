
#ifndef _CPB_H_
#define _CPB_H_

#define CPB_MAGIC       0x00425043    /* 'CPB\0' */
#define CPB_VERSION     4

#define CPB_KEY_TYPE_CARD32             1
#define CPB_KEY_TYPE_STRING             2
#define CPB_KEY_TYPE_ENCRYPTEDSTRING    3

struct CPBKey {
    unsigned keyIndex;
    char   * keyName;
    int      optionType;
};

#define CPB_KEY_HeapLimit                   0x01
#define CPB_KEY_StackLimit                  0x02
#define CPB_KEY_EmitHistory                 0x03
#define CPB_KEY_GenProfile                  0x04
#define CPB_KEY_VMProperty                  0x05
#define CPB_KEY_MainClass                   0x06
#define CPB_KEY_Resource                    0x07
#define CPB_KEY_ClassPath                   0x08
#define CPB_KEY_NoStackTrace                0x09
#define CPB_KEY_NoUsageList                 0x0A
#define CPB_KEY_NTService                   0x0B
#define CPB_KEY_SingleComp                  0x0C
#define CPB_KEY_RTComponent                 0x0D
#define CPB_KEY_StandaloneResources         0x0E
#define CPB_KEY_DLLRedirection              0x0F
#define CPB_KEY_JREHome                     0x10
#define CPB_KEY_JETRuntime                  0x11
#define CPB_KEY_NoSavingClasses             0x12
#define CPB_KEY_JETProfile                  0x13
#define CPB_KEY_BootClassPath               0x14
#define CPB_KEY_VersionInfo                 0x15
#define CPB_KEY_SMPRuntimeFlag              0x16
#define CPB_KEY_RuntimeKind                 0x17
#define CPB_KEY_RuntimeLicenses             0x18
#define CPB_KEY_JETProfileName              0x19
#define CPB_KEY_CompatibilityString         0x1A
#define CPB_KEY_HaveSplash                  0x1B
#define CPB_KEY_SplashMinTime               0x1C
#define CPB_KEY_SplashCloseOnTitle          0x1D
#define CPB_KEY_PackagingOptions            0x1E
#define CPB_KEY_JITCacheComponent           0x1F
#define CPB_KEY_SplashCloseOnAWTWindow      0x20
#define CPB_KEY_SplashCloseOnClick          0x21
#define CPB_KEY_ERCPWindowingSystem         0x22
#define CPB_KEY_IgnoreJETVMPROP             0x23
#define CPB_KEY_MultiMain                   0x24
#define CPB_KEY_ERCPVersion                 0x25      

#define CPB_OPTIONS                         0x26

struct CPBHeader {
    unsigned  magic;
    unsigned  CPBVersion;
    unsigned  jetVerMajor;
    unsigned  jetVerMinor;
    unsigned  jetEdition;
    unsigned  vcode;
    unsigned  nParams;
    unsigned  encryptionKey;
    unsigned  linkerDataVersion;
};


struct CPBParam {
    unsigned keyIndex;
    unsigned valueLen;
};

struct CPBParam_CARD32 {
    struct CPBParam p;
    unsigned value;
};

struct CPBParam_STRING {
    struct CPBParam p;
    char value [1];          // actually char value[valueLen];
};

struct CPBParam_ENCRYPTEDSTRING {
    struct CPBParam p;
    char value [1];          // actually char value[valueLen];
};

/*
 * JET Editions
 */
#define JET_EDITION_UNKNOWN               0

#define JET_EDITION_EVALUATION            2
#define JET_EDITION_PROFESSIONAL          3
#define JET_EDITION_STANDARD              4
#define JET_EDITION_ENTERPRISE            5
#define JET_EDITION_EMBEDDED              6
#define JET_EDITION_EMBEDDED_EVALUATION   7

#endif // _CPB_H_

#ifdef CPB_OPTIONS_DECLARATION

struct CPBKey CPBKeys [CPB_OPTIONS] = {
    {0x00,                           "",                       -1},
    {CPB_KEY_HeapLimit,              "HeapLimit",              CPB_KEY_TYPE_CARD32},
    {CPB_KEY_StackLimit,             "StackLimit",             CPB_KEY_TYPE_CARD32},
    {CPB_KEY_EmitHistory,            "EmitHistory",            CPB_KEY_TYPE_CARD32},
    {CPB_KEY_GenProfile,             "GenProfile",             CPB_KEY_TYPE_CARD32},
    {CPB_KEY_VMProperty,             "VMProperty",             CPB_KEY_TYPE_STRING},
    {CPB_KEY_MainClass,              "MainClass",              CPB_KEY_TYPE_ENCRYPTEDSTRING},
    {CPB_KEY_Resource,               "Resource",               CPB_KEY_TYPE_STRING},
    {CPB_KEY_ClassPath,              "ClassPath",              CPB_KEY_TYPE_STRING},
    {CPB_KEY_NoStackTrace,           "NoStackTrace",           CPB_KEY_TYPE_CARD32},
    {CPB_KEY_NoUsageList,            "NoUsageList",            CPB_KEY_TYPE_CARD32},
    {CPB_KEY_NTService,              "NTService",              CPB_KEY_TYPE_STRING},
    {CPB_KEY_SingleComp,             "SingleComp",             CPB_KEY_TYPE_CARD32},
    {CPB_KEY_RTComponent,            "RTComponent",            CPB_KEY_TYPE_STRING},
    {CPB_KEY_StandaloneResources,    "StandaloneResources",    CPB_KEY_TYPE_STRING},
    {CPB_KEY_DLLRedirection,         "DLLRedirection",         CPB_KEY_TYPE_STRING},
    {CPB_KEY_JREHome,                "JREHome",                CPB_KEY_TYPE_STRING},
    {CPB_KEY_JETRuntime,             "JETRuntime",             CPB_KEY_TYPE_STRING},
    {CPB_KEY_NoSavingClasses,        "NoSavingClasses",        CPB_KEY_TYPE_CARD32},
    {CPB_KEY_JETProfile,             "JETProfile",             CPB_KEY_TYPE_STRING},
    {CPB_KEY_BootClassPath,          "BootClassPath",          CPB_KEY_TYPE_STRING},
    {CPB_KEY_VersionInfo,            "VersionInfo",            CPB_KEY_TYPE_STRING},
    {CPB_KEY_SMPRuntimeFlag,         "SMPRuntimeFlag",         CPB_KEY_TYPE_CARD32},
    {CPB_KEY_RuntimeKind,            "RuntimeKind",            CPB_KEY_TYPE_STRING},
    {CPB_KEY_RuntimeLicenses,        "RuntimeLicenses",        CPB_KEY_TYPE_STRING},
    {CPB_KEY_JETProfileName,         "JETProfileName",         CPB_KEY_TYPE_STRING},
    {CPB_KEY_CompatibilityString,    "CompatibilityString",    CPB_KEY_TYPE_STRING},
    {CPB_KEY_HaveSplash,             "HaveSplash",             CPB_KEY_TYPE_CARD32},
    {CPB_KEY_SplashMinTime,          "SplashMinTime",          CPB_KEY_TYPE_CARD32},
    {CPB_KEY_SplashCloseOnTitle,     "SplashCloseOnTitle",     CPB_KEY_TYPE_STRING},
    {CPB_KEY_PackagingOptions,       "PackagingOptions",       CPB_KEY_TYPE_STRING},
    {CPB_KEY_JITCacheComponent,      "JITCacheComponent",      CPB_KEY_TYPE_CARD32},
    {CPB_KEY_SplashCloseOnAWTWindow, "SplashCloseOnAWTWindow", CPB_KEY_TYPE_CARD32},
    {CPB_KEY_SplashCloseOnClick,     "SplashCloseOnClick",     CPB_KEY_TYPE_CARD32},
    {CPB_KEY_ERCPWindowingSystem,    "ERCPWindowingSystem",    CPB_KEY_TYPE_STRING},
    {CPB_KEY_IgnoreJETVMPROP,        "IgnoreJETVMPROP",        CPB_KEY_TYPE_CARD32},
    {CPB_KEY_MultiMain,              "MultiMain",              CPB_KEY_TYPE_CARD32},
    {CPB_KEY_ERCPVersion,            "ERCPVersion",            CPB_KEY_TYPE_STRING},
};

#else

extern struct CPBKey CPBKeys[];

#endif
