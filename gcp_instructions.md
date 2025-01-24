## IMPORTANT: there are no download protections from GCP. Be careful to avoid downloading high-risk data to local machines, or pushing high-risk data to GitHub.

### Launching VM

1. "Start" the VM on the GCP VM Instances page
2. `ssh` to the VM using terminal on local machine
	* You can copy the base `ssh` gcloud command from the ssh dropdown menu. Add the port you will use to this command (eg., `-- -L 8080:localhost:8080'`).
	* Access the VM through a browser (eg., `localhost:8080`).
3. When you are done with the VM, be sure to "Stop" it on the GCP VM Instances page to stop billing.

### VM Disk Space

Because the Medicare datasets are large, you may need to increase the size of the VM disk. You can find the name and size of the "additional disk" that governs VM storage by clicking on your VM instance name. You can edit the size of the disk via the "Disks" page within the Compute Engine page hierarchy.

After you change the size of the disk, you will also need to update this information on your VM so that memory availability measures are accurate:

* `df -h`: lists disk names
* `sudo resize2fs <diskname>`: update size of disk

Once a disk size is increased, it cannot be decreased. Disk space is relatively cheap compared to running a VM.

### Changing VM Specs

Running VMs can be expensive. To manage costs, I adjust my VM machine type to meet the needs of the specific analysis. 

Base machine type: `n1-highmem-8`

* This machine type has 8 vCPUs and 52 GB RAM. It costs ~$300 to run continuously for one month.

Machine type for 10-fold cross-validation: `n1-highmem-32`

* This machine type has 32 vCPUs and 208 GB RAM. It costs ~$1100 to run continuously for one month.

You can change the machine type of a VM by clicking on the VM name, selecting "edit," and updating the machine configuration.

### Transferring Data Between Storage Bucket and VM

To transfer data between a storage bucket and a VM, from the VM terminal, use `gsutil cp` commands: [https://cloud.google.com/storage/docs/gsutil/commands/cp](https://cloud.google.com/storage/docs/gsutil/commands/cp)
