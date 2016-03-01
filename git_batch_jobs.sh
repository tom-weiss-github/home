# at hh::mm ~/git_batch_jobs.sh
echo Starting Git Batch Jobs >> /tmp/p.log 2>&1
cd /home/tweiss/dev-root/alternate
git tag -d $(git tag) >> /tmp/p.log 2>&1
git fetch -v >> /tmp/p.log 2>&1
git gc --aggressive >> /tmp/p.log 2>&1
echo Finished Git Batch Jobs >> /tmp/p.log 2>&1

