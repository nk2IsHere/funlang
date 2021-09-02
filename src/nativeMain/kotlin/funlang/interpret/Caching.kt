package funlang.interpret

import com.soywiz.korio.file.std.applicationVfs
import kotlinx.collections.immutable.PersistentMap
import kotlinx.collections.immutable.toPersistentMap
import kotlinx.serialization.*
import kotlinx.serialization.builtins.MapSerializer
import kotlinx.serialization.cbor.Cbor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.descriptors.SerialKind
import kotlinx.serialization.descriptors.StructureKind
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder

internal class MapLikeDescriptor(
    override val serialName: String,
    val keyDescriptor: SerialDescriptor,
    val valueDescriptor: SerialDescriptor
) : SerialDescriptor {
    override val kind: SerialKind get() = StructureKind.MAP
    override val elementsCount: Int = 2
    override fun getElementName(index: Int): String = index.toString()
    override fun getElementIndex(name: String): Int =
        name.toIntOrNull() ?: throw IllegalArgumentException("$name is not a valid map index")

    override fun isElementOptional(index: Int): Boolean {
        require(index >= 0) { "Illegal index $index, $serialName expects only non-negative indices"}
        return false
    }

    override fun getElementAnnotations(index: Int): List<Annotation> {
        require(index >= 0) { "Illegal index $index, $serialName expects only non-negative indices"}
        return emptyList()
    }

    override fun getElementDescriptor(index: Int): SerialDescriptor {
        require(index >= 0) { "Illegal index $index, $serialName expects only non-negative indices"}
        return when (index % 2) {
            0 -> keyDescriptor
            1 -> valueDescriptor
            else -> error("Unreached")
        }
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is MapLikeDescriptor) return false
        if (serialName != other.serialName) return false
        if (keyDescriptor != other.keyDescriptor) return false
        if (valueDescriptor != other.valueDescriptor) return false
        return true
    }

    override fun hashCode(): Int {
        var result = serialName.hashCode()
        result = 31 * result + keyDescriptor.hashCode()
        result = 31 * result + valueDescriptor.hashCode()
        return result
    }

    override fun toString(): String = "$serialName($keyDescriptor, $valueDescriptor)"
}


@Serializer(forClass = PersistentMap::class)
class PersistentMapSerializer<K, V>(
    private val keySerializer: KSerializer<K>,
    private val valueSerializer: KSerializer<V>
) : KSerializer<PersistentMap<K, V>> {

    override val descriptor: SerialDescriptor = MapLikeDescriptor(
        "kotlinx.serialization.immutable.persistentMap",
        keySerializer.descriptor,
        valueSerializer.descriptor
    )

    override fun serialize(encoder: Encoder, value: PersistentMap<K, V>) =
        MapSerializer(keySerializer, valueSerializer).serialize(encoder, value.toMap())

    override fun deserialize(decoder: Decoder): PersistentMap<K, V> =
        MapSerializer(keySerializer, valueSerializer).deserialize(decoder).toPersistentMap()
}

private val format = Cbor {  }

suspend fun saveEnv(env: IREnv, path: String, key: String) {
    if(!applicationVfs[path].exists())
        applicationVfs[path].mkdir()

    applicationVfs["$path/$key.cfun"].writeBytes(format.encodeToByteArray(env))
}

suspend fun loadEnv(path: String, key: String): IREnv? {
    if(!applicationVfs["$path/$key.cfun"].exists())
        return null

    return format.decodeFromByteArray<IREnv>(applicationVfs["$path/$key.cfun"].readBytes())
}
