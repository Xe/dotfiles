gcutil adddisk --zone="us-central1-a" \
	--disk_type=pd-standard  \
	--size_gb=32 \
	$1-docker

gcutil addinstance --zone="us-central1-a" \
	--machine_type="g1-small"  \
	--network="default" \
	--external_ip_address="ephemeral" \
	--service_account_scopes="https://www.googleapis.com/auth/devstorage.read_only" \
	--image="https://www.googleapis.com/compute/v1/projects/coreos-cloud/global/images/coreos-alpha-410-0-0-v20140818" \
	--persistent_boot_disk="true" \
	--auto_delete_boot_disk="true" \
	--metadata_from_file=user-data:cloud-config.yaml \
	--tags devel \
	--service_account_scopes="https://www.googleapis.com/auth/devstorage.read_only" \
	--disk="$1-docker,deviceName=xena-vm,mode=READ_WRITE" $1
