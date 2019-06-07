/* Compiler to shell interface */

#define MSG_TEXT	'T'	/* do not display line/col and do not jump to it*/
#define MSG_NOTICE      'N'	/* display line/col and jump, but do not count as error */
#define MSG_WARNING     'W'
#define MSG_ERROR       'E'
#define MSG_SEVERE      'S'

typedef char ERRCLASS;

/* Connect to shell; returns 1 on success. */

int	ConnectShell (void);

/* Disconnect shell */

void    DisconnectShell (void);

/* Send a message "body" in file "filename", line y, column x,
   error class err_class.
   File name or message body can be empty strings or NULL.
   If file name is not empty, it should contain absolute path.
   Possible classes:
   MSG_TEXT    - just some string to be drawn in messages window.
   MSG_NOTICE  - message that corresponds to some place in a file but is not counted
                 as error
   MSG_WARNING - warning, message that does not affect results of compiling.
   MSG_ERROR   - error
   MSG_SEVERE  - severe error, usually error that makes further making
                 impossible
*/

void	SendError (ERRCLASS err_class, long err_no, long x, long y,
		   char * filename, char * body);

/* Send the string to output window */

void	SendString  (char * s);

/* Send global caption (for example, "Making project xxx") */

void	SendCaption (char * s);

/* Send current caption (for example, "Compiling xxx.mod", or
                         "Linking xxx.exe") */

void	SendComment (char * s);

/* Start a job and draw progress indicator;
   progress_limit - maximal value of progress indicator
   comment - string to be drawn to the left of progress indicator
	     (for example, "Lines compiled: 1250").
	     Can contain "%ld" in place of current progress.
*/

void	SendStartJob (char * comment, long progress_limit);

/* Move progress indicator and increase the number in the comment.
   When comment_progress and progress are non-negative, thay are treated
   as offsets to current values.
   Otherwice, their absolute values are treated as new values.
*/

void	SendProgress (long comment_progress, long progress);

/* Instruct shell to clear its currently cached project file list and
   prepare to receive new file list
*/

void	SendStartFileList (void);

/* Send next file name to be added to project files list. Files can be
   sent many times, or out of order, with either '/' or '\\' as path separator.
*/

void	SendFileName (char * name);

/* Notify shell that file list is complete. If compiling is terminated
   before this point, and therefore this function is never called, shell treats
   received file list as incomplete and warns about it in window header
*/

void	CommitFileList (void);


/* Set mode. List of allowed modes follows */

void	SetMode (char mode);

#define SORT_ON		'S'	/* turn on messages sorting by line numbers */
#define SORT_OFF	's'	/* turn it off */

