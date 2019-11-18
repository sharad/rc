#!/usr/bin/env zsh



foreach p ( "$@" ) {

    partition=$p
    bkp=$(basename $partition).crypthdrbkp.img
    
    if [ ! -b $partition ] ; then
        echo $partition is not block device >&2
        continue
    fi

    offset=$(cryptsetup luksDump /dev/sda9 | grep "Payload offset" | awk '{ print $NF }')

    dd if=$partition of=$bkp bs=512 count=$offset

    cat <<EOF
	Backup of Luks header of $partition generated in $bkp
	
	Use below command to restore
	dd if=$bkp of=$partition bs=512 count=$offset
EOF

}

