/*////////////////DOCUMENTATION///////////////////////
////////////////////////////////////////////////////

>format <disk number> <disk unit name>

>mount <disk number> <disk unit name>

>create <block size number> <name> <type>

>ls                         //list the root directory contents

>open <name> <direction>      //direction is w or r

>write                    //begin writing

>close           //close file after reading or writing

>read           //display contents of the file

>delete <filename>

>dismount


//////////////////////////////////////////////////////
/////////////////////////////////////////////////////*/


import "io"

let make_heap() be 
{ let begin = (((! 0x101) >> 10) + 1) << 10, end = ! 0x100;
  init(begin, end - begin); }


let str_eq(a, b) be				             
{  							    
	let i = 0;
  	while true do
  	{ 
		let ca = byte i of a, cb = byte i of b;      
    		if ca <> cb then 			    
      		resultis false;				   
    		if ca = 0 then                             
      		resultis true;				  
	    i +:= 1                             
	}
}

let string_vtof(s, fs, chars) be
{ 
	let i = 0, c;
	while i < chars do                 
  	{ 
		c := byte i of s;          
    		if c = 0 then break;        
    		byte i of fs := c;         
    		i +:= 1                    
	}
  	
	while i < chars do                 
  	{ 
		byte i of fs := 0;
	        i +:= 1 
	}
 }

let string_ftov(fs, s, chars) be             	
{ 
	let i = 0, c;                          
  	while i < chars do                     
	{ 
		c := byte i of fs;            
	        if c = 0 then break;           
                byte i of s := c;             
	        i +:= 1 
	}
	 byte i of s := 0                   
}

let zerob(b) be
{				    
	for i = 0 to 127 do
        b ! i := 0;
}

let atol(s) be     
{ 
	let i = 0, n = 0;
	while true do
        { 
		let c = byte i of s;            
    		if c = 0 then break;              
    		n := n * 10 + c - '0';          
	        i +:= 1                                       
	}
	  resultis n            
   }

let out_time(t) be    
{ 
	let v = vec 7;
  	datetime(t, v);
        out("%d-%02d-%02d %02d:%02d:%02d", v!0, v!1, v!2, v!4, v!5, v!6)
 }

let instring() be
{ let value = 0;
  let i = 0;
 while true do
 { let char = inch();
 if char < 'A' \/ char > 'z' then
 resultis value;
 byte  i of value := char; 
	i+:=1;} } 

manifest
{                 ///////////// superblock contents
  
  sb_numblocks = 0,     // size of disc, how many blocks long
  sb_rootdir = 1,       // which block is the root directory in
  sb_firstfree = 2,
  sb_datemod = 3;                	
  sb_name = 16;
  sb_namelen = 4;
  sb_disknum = 8;
  sb_rootpoint = 9;       //pointer to the root directory in memory
  sb_size = 10; //words

                  //////////// directory entries
  
  de_name = 0,          // name comes first, up to 12 chars, and "fs"
  de_block = 4,         // de ! 3 = the file's header block number
  de_blocklen = 5;      //block length of the file
  de_type = 6;          //type of the file
  de_datemod = 7;       //date modified
  de_namelen = 16,      
  de_namewords = 4,  
  de_size = 8,       

		///////////////file header block contents
  
  hdr_perm = 0;             //1 or 0 for ability to write
  hdr_startblock = 1;       //first block that contains actual file data
  hdr_lenbytes = 2;         //how many bytes the file contains in total
  hdr_lenblks = 3;          //length in block of the file
  hdr_buffer = 4,          // address of the 512 byte buffer
  hdr_block = 5,            // which disc block contains the file's header
  hdr_curr_block = 6,       // which block is currently in the buffer
  hdr_max_block = 7,        // last block the file may occupy
  hdr_buff_pos = 8,        // current position within the buffer
  hdr_buff_max = 9,        // maximum value for buff_pos when file closed from writing
  hdr_can_write = 10,      // true means writable, false means readable
  hdr_curr_wblk = 11,     //the last block of the file that was written into, used as stopping point for reading
  hdr_size = 12
 }

static
{          	             			 
	disk = nil,        //pointer to the super block in memory
        filepnt = nil      //pointer to the file header block in memory
}

let write_block(bn, mem, unit_number) be                       					   
{ 
	let r = devctl(dc_disc_write, unit_number, bn, 1, mem); 				   
  	if r < 1 then 						                                    
    	out("Error %d on trying to write block %d\n", r, bn)
 }
 
let read_block(bn, mem, unit_number) be            
{ 
	let r = devctl(dc_disc_read, unit_number, bn, 1, mem);
  	if r < 1 then
    	out("Error %d on trying to read block %d\n", r, bn)
 }


let format(unit_num, name) be          						  
{
	let superblock;
	let rootdir;
	
	let r = devctl(dc_disc_check, unit_num);			          
	out("%d blocks avalable\n", r);            

    	superblock := newvec(128);         			
  	zerob(superblock);               				         
  	
        superblock ! sb_numblocks := r;    				       
  	superblock ! sb_rootdir := 1;       				     
  	superblock ! sb_firstfree := 2;     				      
  	superblock ! sb_datemod := seconds();
	string_vtof(name, superblock + sb_name, sb_namelen);
	superblock ! sb_disknum := unit_num;
	
	write_block(0, superblock,unit_num);             						
  
    	rootdir := newvec(128);        
  	zerob(rootdir);                     
  	
	write_block(1, rootdir,unit_num)            
}

let mount(unit_number, name) be
{ 
	let testname = vec(sb_namelen);	
	let superblock;
	let rootdir;	
	superblock := newvec(128);   
	zerob(superblock);   	
	
	read_block(0, superblock, unit_number);                  				       
  	string_ftov(superblock + sb_name,testname , sb_name);
	
	unless str_eq(testname, name) do
	{
		out("The disk number does not match the name!\n");
		out("The name is: %s\n\n", testname);	
		resultis 0;
	}
	
	out("\nName of disk: %s\n", superblock + sb_name);
	out("The disk number: %d\n", superblock ! sb_disknum);
	out("%d blocks availible\n", superblock ! sb_numblocks - (superblock ! sb_firstfree));
  	out("first free block %d\n", superblock ! sb_firstfree);
	out("time of formating: ");
	out_time(superblock ! sb_datemod);
	out("\n");
  	out("root dir in block %d\n\n", superblock ! sb_rootdir);
  
    	rootdir := newvec(128);
  	zerob(rootdir);
	read_block(superblock ! sb_rootdir, rootdir,unit_number);					   
	
	superblock ! sb_rootpoint := rootdir;     //super block in memory holds pointer to rootdirector in memory
	resultis superblock;

}

let dismount(disk) be
{
	write_block(0, disk,  disk ! sb_disknum); 					  //write the contents of superblock in memory into block 0
  	write_block(1,  disk ! sb_rootpoint ,  disk ! sb_disknum);		      //write the contents of the root directort into block 1
	out("The disk has been dismounted\n");
}

let ls(disk) be
{
	let name = vec(de_namewords);
	let pde;
	let type = vec(1);
	let tester;
	tester := "del";
	pde := ! (disk ! sb_rootpoint);
	
	out("Root directory contents:\n");
	while (pde ! 0 <> 0) do
	{
		string_ftov(pde + de_name, name, de_namelen);
		string_ftov(pde + de_type, type, 4);
		
		if (str_eq(type,tester)) then 
		{
			pde +:= de_size;
			loop;
		}		

		out("\nname: %s\n startblock: %d \n length in blocks: %d \n type: %s\n",name,pde ! de_block,pde ! de_blocklen,type);
		out("date modified/created: ");
		out_time(pde ! de_datemod);
		out("\n\n"); 
		pde +:=  de_size;
	}
	
}

let create(bsz,fn, type, disk) be     								
{ 
	let pde, i, hdr = newvec(128);							         
	
	i := 0;				
	pde := ! (disk ! sb_rootpoint);	
	while i < 64 do                   
  	{ 
	        if pde ! i = 0 then break;							   
    		i +:= de_size;   								
	 }
	pde := pde ! i;
  
	if i >= 128 then
  	{ 
		outs("root directory is full\n");						    
	        resultis -1 
	}
	string_vtof(fn, pde + de_name, de_namelen); 							     

	pde ! de_block := disk ! sb_firstfree;						    
	disk ! sb_firstfree +:= bsz +1;        						     

	pde ! de_blocklen := bsz;
	pde ! de_datemod := seconds(); 
	
	string_vtof(type, pde + de_type, 4);
	
	
	zerob(hdr);

        hdr ! hdr_startblock := (pde ! de_block) + 1;
        hdr ! hdr_lenbytes := 0;
        hdr ! hdr_lenblks := pde ! de_blocklen;
	hdr ! hdr_buffer := newvec(128);                    	
	zerob(hdr ! hdr_buffer);
	hdr ! hdr_block := pde ! de_block;        
	hdr ! hdr_curr_block := (pde ! de_block) + 1;                       
	hdr ! hdr_max_block := (pde ! de_block) + (pde ! de_blocklen);                    
	hdr ! hdr_buff_pos := 0;                                     
	hdr ! hdr_buff_max := 512;
	hdr ! hdr_curr_wblk := hdr ! hdr_startblock;

	write_block(pde ! de_block, hdr, disk ! sb_disknum);

}

let open(disk, file_name, direction) be
{
	let hdr = newvec(128);
	let pde;
	let datablk = newvec(128);
	let name = vec(de_namewords);
	pde := !(disk ! sb_rootpoint);
	zerob(hdr);
	 while (pde ! 0 <> 0) do
        {
                string_ftov(pde + de_name, name, de_namelen);
             	if  str_eq(file_name, name) then
		{
			out("File open\n");
			break;
		}
	        pde +:=  de_size;
		if pde ! 0 = 0 then
		{
			out("File not found\n");
			resultis nil;
		}
        }
	read_block(pde ! de_block, hdr, disk ! sb_disknum);
	hdr ! hdr_perm := direction;
        if str_eq(direction,"r") then
	{
		read_block(hdr ! hdr_startblock, hdr ! hdr_buffer, disk ! sb_disknum);
		hdr ! hdr_curr_block := hdr ! hdr_startblock;
		hdr ! hdr_buff_pos := 0; 
		hdr ! hdr_can_write := 0;
		
	}
									
	if str_eq(direction,"w") then
	{
		hdr ! hdr_curr_block := hdr ! hdr_curr_wblk;
		read_block(hdr ! hdr_curr_block, hdr ! hdr_buffer, disk ! sb_disknum);
		hdr ! hdr_can_write := 1;
		pde ! de_datemod := seconds();
		test hdr ! hdr_buff_max = 512 then
                {hdr ! hdr_buff_pos := 0;}
		else
		{hdr ! hdr_buff_pos := hdr ! hdr_buff_max;}
	
	}
	resultis hdr;
}

let delete(disk,file_name) be
{
	let type = "del";
	let name = vec(de_namewords);
	let pde = !(disk ! sb_rootpoint);
	 while (pde ! 0 <> 0) do
        {
                string_ftov(pde + de_name, name, de_namelen);
                if  str_eq(file_name, name) then
                {
                        break;
                }
                pde +:=  de_size;
        }
	if(pde ! 0 = 0) then
	{
		out("File not found for deletion\n");
		return;
	}
	
	string_vtof(type, pde + de_type, 4); 
	out("File deleted\n");
}
let close(file,disk) be
{
	if file ! hdr_can_write = 1 then
	{
		file ! hdr_lenbytes +:= file ! hdr_buff_pos;
		file ! hdr_buff_max := file ! hdr_buff_pos;
		write_block(file ! hdr_block, file, disk ! sb_disknum);
		write_block(file ! hdr_curr_block, file ! hdr_buffer, disk ! sb_disknum);
	}
	if file ! hdr_can_write = 0 then
		write_block(file ! hdr_block, file, disk ! sb_disknum);
	out("\nFile closed\n");
}

let eof(file) be
{
        if (file ! hdr_lenbytes) = (512 * (file ! hdr_lenblks))  then 
	  resultis true;
        
	resultis false;
}

let file_empty_buffer(file) be     //increments the lenght in bytes, writes buffer to disk, updates last writing block
{ 
file ! hdr_lenbytes +:= file ! hdr_buff_pos;
  write_block(file ! hdr_curr_block, file ! hdr_buffer, disk ! sb_disknum);

if file ! hdr_curr_block = file ! hdr_max_block then 
{ 
	outs("file maximum is reached, bytes might have been cut off if input exceded file capacity\n");
        resultis 0
}

  zerob(file ! hdr_buffer);
  file ! hdr_curr_block +:= 1;
  file ! hdr_buff_pos := 0;
  file ! hdr_buff_max := 512;
  file ! hdr_curr_wblk := file ! hdr_curr_block;
  resultis 1 
}

let fwrite_char(file, c) be
{ 
	if file ! hdr_can_write = 0 then
	{
		out("You cannot write to a file open to read, writing failed\n");
		return;
	}
	byte file ! hdr_buff_pos of file ! hdr_buffer := c;
  	file ! hdr_buff_pos +:= 1;
  	if file ! hdr_buff_pos >= 512 then
	{	
		file_empty_buffer(file);
	}
	resultis c 
}

let write_to_file(file) be
{ 
	let c;
	while true do
  	{
		 c := inch();
    		if c = '*' then break;
		if eof(file) then
		{
		 	return;
		}
    		fwrite_char(file, c);
	}
  	while c <> '\n' do
	  c := inch()
}

let file_refill_buffer(file) be
{ 
  file ! hdr_curr_block +:= 1;
  file ! hdr_buff_pos := 0;
  if file ! hdr_curr_block > file ! hdr_max_block then
    resultis 0;
  read_block(file ! hdr_curr_block, file ! hdr_buffer,disk ! sb_disknum);
}

let fread_char(file) be
{	
  let c; 
  if file ! hdr_can_write = 1 then
  { 
	out("file* not readable\n");
        resultis -1
  }
  
  if file ! hdr_buff_pos >= file ! hdr_buff_max /\ file ! hdr_curr_block = file ! hdr_curr_wblk then
  { 
	out("\n\nYou have reached the end of readable contents\n\n"); 
	resultis -1;
  }
  if file ! hdr_buff_pos >= 512 then
    file_refill_buffer(file); 

  c := byte file ! hdr_buff_pos of file ! hdr_buffer;
  
  file ! hdr_buff_pos +:= 1;
  resultis c 
}


let read_from_file(file, n) be
{ let c;
  if n = 0 then n := 0x7FFFFFFF;
  while n > 0 do
  { 
	c := fread_char(file);
        if c = -1 then break;
        outch(c);
        n -:= 1 
  }
  outch('\n') 
}


let command(parts, np) be
{ let p0 = parts ! 0, p2 = "";
  let p1 = "",p3 = "", p4 = "", p5 = "";
  let result;
  if np > 1 then p1 := parts ! 1;
  if np > 2 then p2 := parts ! 2;
  if np > 3 then p3 := parts ! 3;
  if np > 4 then p4 := parts ! 4;
  if np > 5 then p5 := parts ! 5;  

  test str_eq(p0, "dismount") then
    dismount(disk)        

  else test str_eq(p0, "format") then
    format(atol(p1),p2)           

  else test str_eq(p0, "mount") then{
    disk := mount(atol(p1),p2);
    if disk = 0 then{
	out("Unsuccessful mount\n");}}
	            
 
 else test str_eq(p0, "create") then
    create(atol(p1), p2, p3,disk)    

  else test str_eq(p0, "ls") then
    ls(disk)

  else test str_eq(p0, "open") then
    filepnt := open(disk,p1,p2) 

  else test str_eq(p0,"write") then
  {
	out("Stop writing by entering '*'\n");
	write_to_file(filepnt);	
	
  }	
  else test str_eq(p0,"read") then
  {
	 read_from_file(filepnt, atol(p2));
  }
  else test str_eq(p0, "delete") then
  {
	delete(disk, p1);
  }
  else test str_eq(p0,"close") then
    close(filepnt,disk)
  else
    out("unrecognised command '%s'\n", p0) }


let command_loop() be
{ manifest
  { max_bytes = 120,
    max_chars = 112,
    max_parts = 6 }
  let line = vec max_bytes/4, part = vec max_parts, nparts, c, i;
  while true do
  { nparts := 0;
    i := 0;
    outs("> ");
    c := inch();
    while true do
    { while c = ' ' do
        c := inch();
      if c = '\n' then 
        break;
      if nparts < max_parts then
      { part ! nparts := line + i/4;
        nparts +:= 1; }
      while c > ' ' do
      { if i < max_chars then
        { byte i of line := c;
          i +:= 1; }
        c := inch() }
      { if i < max_bytes then
          byte i of line := 0;
        i +:= 1 }
          repeatuntil i rem 4 = 0; }
    command(part, nparts) } }

let start() be
{
	make_heap();
	command_loop();
}
