#ifndef __HEADWIN__
#define __HEADWIN__
#include <OS2.H>

// Регистрялка классов окон
//
BOOL InitHeadwin(HAB hAB);

//////////////////////////////////////////////////////////////////////////////
//
// Класс окна WC_HEADWIN - набор из расколоженных по горизонтали кнопок.
// Поддерживает перетаскивание ширины кнопок, при этом посылает WM_CONTROLы.
// Размер кнопок может меняться и обычным образом, можно их и уничтожать.
//

    #define WC_HEADWIN "WCHeadwin"
    #define PP_FONT    "8.Helv"

    enum HMODES
    {
      HMOD_DRAGABLE,    // Установка при создании и драгание мышой (дефолтный)
      HMOD_NODRAGABLE,  // Установка при создании, драгание запрещено
      HMOD_TASKBAR      // Авто-размещение, драгание запрещено (для таскбара)
    };

    struct HBTNCREATESTRUCT
    {
      PSZ     pszText;     // Текст кнопки
      LONG    lWidth;      // Ширина (точек)
      USHORT  usCmd;       // Идентификатор
      USHORT  usCmdBefore; // Вставить перед (нету - в конец)
    };
    typedef HBTNCREATESTRUCT *PHBTNCREATESTRUCT;


    // Добавить кнопку
    //
    // m1 - PHBTNCREATESTRUCT
    // Returns: хэндл кнопки или 0
    //
    #define HM_ADDBUTTON      WM_USER+1000


    // Режим управления размерами кнопок
    //
    // m1 - режим из HMODES
    //
    #define HM_SIZEMODE       WM_USER+1001


    // Спросить ширину кнопки
    //
    // SHORT1FROMMP(m1) - usCmd или порядковый номер кнопки
    // SHORT2FROMMP(m1) - TRUE/FALSE == usCmd/номер
    // Returns: ширина или -1 при ошибке
    //
    #define HM_QUERYBTNWIDTH  WM_USER+1002


    // Установить ширину кнопки
    //
    // SHORT1FROMMP(m1) - usCmd или порядковый номер кнопки
    // SHORT2FROMMP(m1) - TRUE/FALSE == usCmd/номер
    // m2               - ширина
    // Returns: ширина или -1 при ошибке
    //
    #define HM_SETBTNWIDTH  WM_USER+1003


    // Спросить оптимальную высоту окна
    //
    #define HM_QUERYOPTHEIGHT WM_USER+1004


    //---- WM_CONTROL:
    #define HBN_TRACKING   BN_PAINT+100 // m2 = usCmd,sWidth - двигается ширина кнопки
    #define HBN_SIZE       BN_PAINT+101 // m2 = usCmd,sWidth - установлена (драгом) ширина кнопки


#endif  /* ifndef __HEADWIN__ */

