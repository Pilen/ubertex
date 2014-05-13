
import java.util.TreeMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.SortedMap;
import java.util.Map;
import java.util.Set;
import java.util.LinkedHashSet;


public class TaskManager implements Runnable{
    private final TaskPerformer performer;
    private TreeMap<String, Set<Task>> tasks;
    private ReentrantLock lock;

    public TaskManager(TaskPerformer performer) {
        this.performer = performer;
        this.lock = new ReentrantLock(true);
        this.lock.lock();
        this.tasks = new TreeMap<String, Set<Task>>();
        this.lock.unlock();
    }

    public void addTask(String time, String command) {
        this.addTask(time, command, "");
    }

    public void addTask(String time, String command, String options) {
        Task task = new Task(command, options);
        this.lock.lock();
        Set<Task> tasksAt = this.tasks.get(time);
        if (tasksAt == null) {
            tasksAt = new LinkedHashSet<Task>();
            this.tasks.put(time, tasksAt);
        }
        tasksAt.add(task);
        this.lock.unlock();
    }

    public int clear() {
        this.lock.lock();
        int size = this.size();
        this.tasks.clear();
        this.lock.unlock();
        return size;
    }

    public int size() {
        int size = 0;
        this.lock.lock();
        for (Set<Task> tasksAt : this.tasks.values()) {
            for (Task task : tasksAt) {
                size++;
            }
        }
        this.lock.unlock();
        return size;
    }

    public void process() {
        this.lock.lock();
        SortedMap<String, Set<Task>> currentTasks = this.tasks.headMap("now");
        for (Map.Entry<String, Set<Task>> entry : currentTasks.entrySet()) {
            Set<Task> tasksAt = entry.getValue();
            this.tasks.remove(entry.getKey());
            for (Task task : tasksAt) {
                this.performer.doTask(task.command, task.options);
            }
        }
        this.lock.unlock();
    }

    public void run() {
        while (true) {
            process();
        }
    }

    private class Task {
        public final String command;
        public final String options;

        public Task(String command) {
            this(command, "");
        }
        public Task(String command, String options) {
            this.command = command;
            this.options = options;
        }
        public boolean equals(Object other) {
            if (other instanceof Task) {
                Task otherTask = (Task) other;
                return this.command.equals(otherTask.command) && this.options.equals(otherTask.options);
            }
            return false;
        }

        public int hashCode() {
            return (this.command + this.options).hashCode();
        }
    }
}
