# SMD Disk Controller

**Documentation for the ND-100 SMD (Storage Module Device) disk controller.**

---

## Files

| File | Contents |
|------|----------|
| [SMD-CONTROLLER-PROGRAMMING-GUIDE.md](SMD-CONTROLLER-PROGRAMMING-GUIDE.md) | Complete register map, I/O sequences, and programming model for the ND-100 SMD controller. Sources: the nd100x emulator SMD device, SINTRAN III NPL source (`IP-P2-DISK-START.NPL`), and SINTRAN boot trace analysis (17,402 IOX accesses, 731 GO commands). |

---

## Related

- [../SCSI/](../SCSI/) - the SCSI disk subsystem (the other main mass-storage path)
- [../../OS/15-DISK-IO-SUBSYSTEM.md](../../OS/15-DISK-IO-SUBSYSTEM.md) - the OS disk I/O layer above the controller

---

**Parent:** [../README.md](../README.md)
