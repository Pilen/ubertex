
import java.util.TreeMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.SortedMap;
import java.util.Map;

public class TaskManager implements Runnable{
    private final Controller controller;
    private TreeMap<String, Task> tasks;
    private ReentrantLock lock;

    public TaskManager(Controller controller) {
        this.controller = controller;
        this.lock = new ReentrantLock(true);
        this.lock.lock();
        this.tasks = new TreeMap<String, Task>();
        this.lock.unlock();
    }

    public void addTask(String time, String command) {
        this.addTask(time, command, "");
    }

    public void addTask(String time, String command, String options) {
        Task task = new Task(command, options);
        this.lock.lock();
        this.tasks.put(time, task);
        this.lock.unlock();
    }

    public void run() {
        while (true) {
            this.lock.lock();
            SortedMap<String, Task> currentTasks = this.tasks.headMap("now");
            for (Map.Entry<String, Task> entry : currentTasks.entrySet()) {
                Task task = entry.getValue();
                this.tasks.remove(entry.getKey());
                this.controller.command(task.command, task.options);
            }
            this.lock.unlock();
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
    }
}
