function fish_title
    set -l command (echo $_)

    if test $command = "fish"
        if git rev-parse --git-dir > /dev/null ^ /dev/null

            set -l git_dir (git rev-parse --git-dir)
            if test $git_dir = ".git"
                echo (basename (pwd))
            else
                echo (basename (dirname $git_dir))
            end
        else
            echo (pwd)
        end
    else
        echo $command
    end
end
