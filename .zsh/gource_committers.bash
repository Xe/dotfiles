git_joined_committers_caption() {
  authors=`git log --no-merges --format='%aN' | sort -u`
  OLD_IFS=$IFS
  IFS=$(echo -en "\n\b")
  author_list=""
  for author in $authors; do
    author_first_commit=`git log --author="${author}" --pretty=format:%H|tail -1`
    author_list+="$(git log -1 --format='%ct|%an started comitting' $author_first_commit)\n"
  done
  IFS=$OLD_IFS
  echo -en $author_list|sort -u
}