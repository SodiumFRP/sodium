import java.awt.Point;

public class Character {
    public Character(int id, CharacterType type, Point pos,
                     Vector velocity) {
        this.id = id;
        this.type = type;
        this.pos = pos;
        this.velocity = velocity;
    }
    public final int id;
    public final CharacterType type;
    public final Point pos;
    public final Vector velocity;
}

