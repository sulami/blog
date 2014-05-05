Title: How to identify bad drives in your RAID array
Date: 2013-05-14 19:13
Author: sulami
Category: Hardware
Tags: hdds, raid
Slug: how-to-identify-bad-drives-in-your-raid-array

If one of the disks in your Linux-driven software-RAID fails, chances
are there are some/a lot of the same disks involved. Especially on
servers there might be a dozen or even more drives of the same type, one
just failed and you need to switch it fast because the server is
critical, so if another drive fails, you loose money and/or lifes.

First do

    cat /proc/mdstat

to verify that exactly one drive has gone bad. There will be an output
similar to this:

    Personalities : [raid1] 
    md0 : active raid1 sdb2[1] sda2[0]
          968567676 blocks super 1.1 [2/2] [UU]
          bitmap: 1/8 pages [4KB], 65536KB chunk

    unused devices: <none>

This is my personal homeserver, running two 1TB-drives in RAID1 (mainly
becuase it is cheap), and both drives are “up”, indicated by the “[UU]“.
Everything but an U in this list is bad. Now you still need to know,
which of the physical drives it is. Even on full hotswappable servers
you probably want to avoid pulling out every single drive and test it in
a different machine.

Enter smartctl. Installed by default on most systems (SUSE, RHEL, …), it
prints hardware info of specified devices. Take a look:

    # smartctl -a /dev/sda

    smartctl 5.43 2012-06-30 r3573 [x86_64-linux-2.6.32-358.0.1.el6.x86_64] (local build)
    Copyright (C) 2002-12 by Bruce Allen, http://smartmontools.sourceforge.net

    === START OF INFORMATION SECTION ===
    Model Family:     Western Digital RE4 Serial ATA
    Device Model:     WDC WD1003FBYX-01Y7B1
    Serial Number:    WD-WMAW30891107
    LU WWN Device Id: 5 0014ee 25c2357fa
    Firmware Version: 01.01V02
    User Capacity:    1,000,204,886,016 bytes [1.00 TB]
    Sector Size:      512 bytes logical/physical
    Device is:        In smartctl database [for details use: -P show]
    ATA Version is:   8
    ATA Standard is:  Exact ATA specification draft version not indicated
    Local Time is:    Sun May 12 17:59:04 2013 CEST
    SMART support is: Available - device has SMART capability.
    SMART support is: Enabled

    === START OF READ SMART DATA SECTION ===
    SMART overall-health self-assessment test result: PASSED

    General SMART Values:                                                                                                                                                                                                                        
    Offline data collection status:  (0x84) Offline data collection activity                                                                                                                                                                     

    Self-test execution status:      (   0) The previous self-test routine completed                                                                                                                                                             

    Total time to complete Offline                                                                                                                                                                                                               
    data collection:                (16260) seconds.                                                                                                                                                                                             
    Offline data collection                                                                                                                                                                                                                      
    capabilities:                    (0x7b) SMART execute Offline immediate.                                                                                                                                                                     

                                            Self-test supported.
                                            Conveyance Self-test supported.
                                            Selective Self-test supported.
    SMART capabilities:            (0x0003) Saves SMART data before entering
                                            power-saving mode.
                                            Supports SMART auto save timer.
    Error logging capability:        (0x01) Error logging supported.
                                            General Purpose Logging supported.
    Short self-test routine 
    recommended polling time:        (   2) minutes.
    Extended self-test routine
    recommended polling time:        ( 160) minutes.
    Conveyance self-test routine
    recommended polling time:        (   5) minutes.
    SCT capabilities:              (0x303f) SCT Status supported.
                                            SCT Error Recovery Control supported.
                                            SCT Feature Control supported.
                                            SCT Data Table supported.

    SMART Attributes Data Structure revision number: 16
    Vendor Specific SMART Attributes with Thresholds:
    ID# ATTRIBUTE_NAME          FLAG     VALUE WORST THRESH TYPE      UPDATED  WHEN_FAILED RAW_VALUE
      1 Raw_Read_Error_Rate     0x002f   200   200   051    Pre-fail  Always       -       0
      3 Spin_Up_Time            0x0027   192   169   021    Pre-fail  Always       -       3366
      4 Start_Stop_Count        0x0032   100   100   000    Old_age   Always       -       65
      5 Reallocated_Sector_Ct   0x0033   200   200   140    Pre-fail  Always       -       0
      7 Seek_Error_Rate         0x002e   200   200   000    Old_age   Always       -       0
      9 Power_On_Hours          0x0032   099   098   000    Old_age   Always       -       1092
     10 Spin_Retry_Count        0x0032   100   253   000    Old_age   Always       -       0
     11 Calibration_Retry_Count 0x0032   100   253   000    Old_age   Always       -       0
     12 Power_Cycle_Count       0x0032   100   100   000    Old_age   Always       -       64
    192 Power-Off_Retract_Count 0x0032   200   200   000    Old_age   Always       -       45
    193 Load_Cycle_Count        0x0032   200   200   000    Old_age   Always       -       19
    194 Temperature_Celsius     0x0022   106   101   000    Old_age   Always       -       41
    196 Reallocated_Event_Count 0x0032   200   200   000    Old_age   Always       -       0
    197 Current_Pending_Sector  0x0032   200   200   000    Old_age   Always       -       0
    198 Offline_Uncorrectable   0x0030   100   253   000    Old_age   Offline      -       0
    199 UDMA_CRC_Error_Count    0x0032   200   200   000    Old_age   Always       -       0
    200 Multi_Zone_Error_Rate   0x0008   100   253   000    Old_age   Offline      -       0

    SMART Error Log Version: 1
    No Errors Logged

    SMART Self-test log structure revision number 1
    No self-tests have been logged.  [To run self-tests, use: smartctl -t]

    SMART Selective self-test log data structure revision number 1
     SPAN  MIN_LBA  MAX_LBA  CURRENT_TEST_STATUS
        1        0        0  Not_testing
        2        0        0  Not_testing
        3        0        0  Not_testing
        4        0        0  Not_testing
        5        0        0  Not_testing
    Selective self-test flags (0x0):
      After scanning selected spans, do NOT read-scan remainder of disk.
    If Selective self-test is pending on power-up, resume after 0 minute delay.

Use it like this as root to get the smart status and serial number of
your device, so you can find the bad one. Now, you either need to stop
and open your server and pull out the right drive, or you are smart like
I am (pun intended) and already got the serial numbers on the outside of
your HDD-slots, sou you can hotswap the drive and start rebuilding right
away.
