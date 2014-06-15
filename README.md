4chin-otaku
===========

It will download all post attachments that aren't already on disk. the default download location is the directory where the program is ran, with a folder created for the board. Files are downloaded in their _4chan 'tim'_ name whichi is the familiar unixtime filename.

# Usage
1. Select board you'd like to download from
2. Select ext of files to download. note some boards only have certain file exts (eg, flash only has swf)
3. Click 'get posts' - be patient, on bigger boards this will take ~15 seconds
4. Download will have the number of new files for the extension in parenthesis, eg 'download (100)'. this means there are 100 new files of the given ext type to download
5. click download to start the download. note this isnt blazing fast for two reasons, not multithreaded (only one file is downloading at a time) and 4chan throttling to prevent banning yourself.
6. If you want to later download only new posts, click 'get posts' again and the count of new files in download button should update. if it stays at 0 there were no new posts for the given file ext.

Picture of usage (do 1, 2, 3, 4!)
![alt text](https://github.com/DexterHaslem/4chin-otaku/raw/master/ui_instr.png "ui pic")

Yes, I know the UI sucks but it is the last thing I will do

### ~ warning ~
if you point this tool at a board of questionable content, you may get some 'interesting' content at an alarming rate (for those of you familiar with 4chan). I cannot be held responsible for what this program fills your hard drive with.
It's also worth noting if you fill your SSD with webms, windows will not boot, it's 2014 and that's still a thing....
